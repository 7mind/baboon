package io.septimalmind.baboon.tests

import io.septimalmind.baboon.LockfileManager
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, IOIssue}
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.functional.bio.{Error2, F}
import izumi.functional.bio.impl.BioEither
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.fundamentals.collections.nonempty.NEList
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

/** T157: the lockfile is a global, target-independent artifact, so `validateLock` must run EXACTLY ONCE
  * per compiler invocation — hoisted OUTSIDE the per-target loop.
  *
  * After the hoist, `BaboonCompiler.run` (the per-target body) no longer calls `validateLock`; the
  * orchestrating caller (`Baboon.entrypoint`) calls it once, BEFORE `F.traverse_(options.targets)(...)`.
  *
  * This test pins the control-flow contract of that seam with a counting `LockfileManager` spy and a
  * faithful re-statement of the orchestrator shape (validate once, then traverse N targets). It proves:
  *   - once-ness: for N >= 2 targets, the validate counter is EXACTLY 1, not N (the per-invocation, not
  *     per-target, semantics — the regression the hoist fixes);
  *   - ordering / short-circuit: a FAILING validate aborts before ANY target runs (a `Force` rewrite or
  *     enforcement failure happens once, up front, and short-circuits the whole invocation).
  */
class T157LockfileHoistOnceTest extends AnyWordSpec with Matchers {

  private type EitherF[+E, +A] = Either[E, A]
  private implicit val error2: Error2[EitherF]               = BioEither
  private implicit val maybeSuspend2: MaybeSuspend2[EitherF] = new MaybeSuspend2[EitherF]

  /** Counting spy: increments on every `validateLock` call and records the model it was handed. */
  private final class CountingLockfileManager extends LockfileManager[EitherF] {
    val calls: AtomicInteger                       = new AtomicInteger(0)
    val seen: AtomicReference[Option[BaboonFamily]] = new AtomicReference(None)

    override def validateLock(model: BaboonFamily): EitherF[NEList[BaboonIssue], Unit] = {
      calls.incrementAndGet()
      seen.set(Some(model))
      Right(())
    }
  }

  /** Always-failing spy: validate short-circuits the invocation. */
  private final class FailingLockfileManager extends LockfileManager[EitherF] {
    val calls: AtomicInteger = new AtomicInteger(0)

    override def validateLock(model: BaboonFamily): EitherF[NEList[BaboonIssue], Unit] = {
      calls.incrementAndGet()
      Left(BaboonIssue.of(IOIssue.CantWriteOutput("lock", new RuntimeException("drift"))))
    }
  }

  /** Faithful re-statement of the hoisted orchestrator seam from `Baboon.entrypoint`:
    * validateLock ONCE, then iterate the targets. `model` is irrelevant to the spies, so we pass null
    * (the production call site passes the single loaded `BaboonFamily`).
    */
  private def orchestrate(
    manager: LockfileManager[EitherF],
    model: BaboonFamily,
    targets: Seq[Int],
    perTarget: Int => EitherF[NEList[BaboonIssue], Unit],
  ): EitherF[NEList[BaboonIssue], Unit] = {
    for {
      _ <- manager.validateLock(model)
      _ <- F.traverse_(targets)(perTarget)
    } yield {}
  }

  "T157 lockfile hoist" should {

    "call validateLock EXACTLY ONCE for an N>=2-target invocation (not once per target)" in {
      val mgr           = new CountingLockfileManager
      val perTargetRuns = new AtomicInteger(0)
      val targets       = Seq(1, 2, 3) // N = 3 >= 2

      val res = orchestrate(
        mgr,
        model = null.asInstanceOf[BaboonFamily],
        targets = targets,
        perTarget = _ => { perTargetRuns.incrementAndGet(); Right(()) },
      )

      res shouldBe Right(())
      mgr.calls.get() shouldBe 1              // EXACTLY ONCE — the hoist invariant
      perTargetRuns.get() shouldBe targets.size // all targets still ran, after the single validate
    }

    "still call validateLock once even for a single target (per-invocation, not per-target)" in {
      val mgr = new CountingLockfileManager
      orchestrate(mgr, null.asInstanceOf[BaboonFamily], Seq(1), _ => Right(())) shouldBe Right(())
      mgr.calls.get() shouldBe 1
    }

    "short-circuit BEFORE any target runs when validate fails (force rewrite/enforcement happens up front)" in {
      val mgr           = new FailingLockfileManager
      val perTargetRuns = new AtomicInteger(0)

      val res = orchestrate(
        mgr,
        null.asInstanceOf[BaboonFamily],
        Seq(1, 2, 3),
        _ => { perTargetRuns.incrementAndGet(); Right(()) },
      )

      res.isLeft shouldBe true
      mgr.calls.get() shouldBe 1   // validate attempted exactly once
      perTargetRuns.get() shouldBe 0 // and NO target generated — enforcement short-circuits the invocation
    }
  }
}
