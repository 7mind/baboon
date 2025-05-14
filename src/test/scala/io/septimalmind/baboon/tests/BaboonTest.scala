package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import distage.{DefaultModule, ModuleDef}
import io.septimalmind.baboon.*
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.util.functional.ParallelAccumulatingOpsInstances
import izumi.distage.modules.DefaultModule2
import izumi.distage.plugins.PluginConfig
import izumi.distage.testkit.model.TestConfig
import izumi.distage.testkit.scalatest.Spec2
import izumi.functional.bio.Exit
import izumi.functional.quasi.{QuasiAsync, QuasiIO, QuasiIORunner, QuasiRef}
import izumi.fundamentals.platform.functional.Identity
import izumi.reflect.TagKK

import java.nio.file.Paths

abstract class BaboonTest[F[+_, +_]: TagKK](implicit baboonTestModule: BaboonTestModule[F]) extends Spec2[F]()(baboonTestModule.defaultModule, implicitly[TagKK[F]]) {
  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(
      new BaboonModule[Either](
        CompilerOptions(
          debug = false,
          target = TargetOptions(
            runtime             = RuntimeGenOpt.With,
            generateConversions = true,
            // dummy path (should be unused)
            output         = Paths.get("./target/baboon-scalatests/"),
            fixturesOutput = None,
            testsOutput    = None,
          ),
          generic = GenericOptions(
            obsoleteErrors           = false,
            codecTestIterations      = 500,
            metaWriteEvolutionJsonTo = None,
          ),
          csOptions = CSOptions(
            writeEvolutionDict                        = true,
            useCompactAdtForm                         = true,
            wrappedAdtBranchCodecs                    = true,
            disregardImplicitUsings                   = true,
            omitMostRecentVersionSuffixFromPaths      = true,
            omitMostRecentVersionSuffixFromNamespaces = true,
            enableDeprecatedEncoders                  = false,
          ),
        ),
        Seq(Paths.get("./src/test/resources/baboon")),
        ParallelAccumulatingOpsInstances.Lawless_ParallelAccumulatingOpsEither,
      ).morph[PluginBase]
    )
  )
}

object BaboonTest {
  final class BaboonTestModule[F[+_, +_]](val defaultModule: DefaultModule2[F])
  object BaboonTestModule {
    implicit val eitherBaboonTestModule: BaboonTestModule[Either] = new BaboonTestModule[Either](EitherDefaultModule)
  }

  private type FThrowable[F[+_, +_]] = { type l[+A] = F[Throwable, A] }

  private implicit final class EitherOps[A](private val value: Either[Throwable, A]) extends AnyVal {
    def getOrThrow: A = value match {
      case Left(value)  => throw value
      case Right(value) => value
    }
  }

  private implicit val quasiAsync: QuasiAsync[FThrowable[Either]#l] = {
    val id = QuasiAsync.quasiAsyncIdentity
    new QuasiAsync[FThrowable[Either]#l] {
      override def async[A](effect: (Either[Throwable, A] => Unit) => Unit): Either[Throwable, A]                  = Right(id.async[A](effect))
      override def parTraverse[A, B](l: IterableOnce[A])(f: A => Either[Throwable, B]): Either[Throwable, List[B]] = Right(id.parTraverse(l)(f(_).getOrThrow))
      override def parTraverse_[A](l: IterableOnce[A])(f: A => Either[Throwable, Unit]): Either[Throwable, Unit]   = Right(id.parTraverse_(l)(f(_).getOrThrow))
      override def parTraverseN[A, B](n: Int)(l: IterableOnce[A])(f: A => Either[Throwable, B]): Either[Throwable, List[B]] = Right(
        id.parTraverseN(n)(l)(f(_).getOrThrow)
      )
      override def parTraverseN_[A](n: Int)(l: IterableOnce[A])(f: A => Either[Throwable, Unit]): Either[Throwable, Unit] = Right(id.parTraverseN_(n)(l)(f(_).getOrThrow))
    }
  }

  private implicit val quasiIO: QuasiIO[FThrowable[Either]#l] = {
    val id = QuasiIO.quasiIOIdentity
    new QuasiIO[FThrowable[Either]#l] {
      override def flatMap[A, B](fa: Either[Throwable, A])(f: A => Either[Throwable, B]): Either[Throwable, B] = id.flatMap(fa.getOrThrow)(f)
      override def guaranteeOnFailure[A](fa: => Either[Throwable, A])(cleanupOnFailure: Throwable => Either[Throwable, Unit]): Either[Throwable, A] =
        id.guaranteeOnFailure(fa)(cleanupOnFailure(_).getOrThrow)
      override def bracketCase[A, B](acquire: => Either[Throwable, A])(release: (A, Option[Throwable]) => Either[Throwable, Unit])(use: A => Either[Throwable, B])
        : Either[Throwable, B] = Right(id.bracketCase[A, B](acquire.getOrThrow)((a: A, o: Option[Throwable]) => release(a, o).getOrThrow)(use(_).getOrThrow))
      override def maybeSuspend[A](eff: => A): Either[Throwable, A]                          = Right(id.maybeSuspend[A](eff))
      override def maybeSuspendEither[A](eff: => Either[Throwable, A]): Either[Throwable, A] = Right(id.maybeSuspendEither[A](eff))
      override def definitelyRecoverUnsafeIgnoreTrace[A](action: => Either[Throwable, A])(recover: Throwable => Either[Throwable, A]): Either[Throwable, A] =
        id.definitelyRecoverUnsafeIgnoreTrace(action)(recover)
      override def definitelyRecoverWithTrace[A](action: => Either[Throwable, A])(recoverWithTrace: (Throwable, Exit.Trace[Throwable]) => Either[Throwable, A])
        : Either[Throwable, A] = id.definitelyRecoverWithTrace(action)(recoverWithTrace)
      override def redeem[A, B](action: => Either[Throwable, A])(failure: Throwable => Either[Throwable, B], success: A => Either[Throwable, B]): Either[Throwable, B] =
        id.redeem(action.getOrThrow)(failure, success)
      override def fail[A](t: => Throwable): Either[Throwable, A]                                      = id.fail(t)
      override def suspendF[A](effAction: => Either[Throwable, A]): Either[Throwable, A]               = id.suspendF(effAction)
      override def tailRecM[A, B](a: A)(f: A => Either[Throwable, Either[A, B]]): Either[Throwable, B] = Right(id.tailRecM[A, B](a)(f(_).getOrThrow))
      override def bracket[A, B](acquire: => Either[Throwable, A])(release: A => Either[Throwable, Unit])(use: A => Either[Throwable, B]): Either[Throwable, B] =
        Right(id.bracket[A, B](acquire.getOrThrow)(release(_).getOrThrow)(use(_).getOrThrow))
      override def guarantee[A](fa: => Either[Throwable, A])(`finally`: => Either[Throwable, Unit]): Either[Throwable, A] = id.guarantee(fa)(`finally`.getOrThrow)
      override def traverse[A, B](l: Iterable[A])(f: A => Either[Throwable, B]): Either[Throwable, List[B]]               = Right(id.traverse(l)(f(_).getOrThrow))
      override def traverse_[A](l: Iterable[A])(f: A => Either[Throwable, Unit]): Either[Throwable, Unit]                 = Right(id.traverse_(l)(f(_).getOrThrow))
      override def pure[A](a: A): Either[Throwable, A]                                                                    = Right(a)
      override def map2[A, B, C](fa: Either[Throwable, A], fb: => Either[Throwable, B])(f: (A, B) => C): Either[Throwable, C] = Right(
        id.map2[A, B, C](fa.getOrThrow, fb.getOrThrow)(f)
      )
      override def map[A, B](fa: Either[Throwable, A])(f: A => B): Either[Throwable, B] = fa.map(f)

      override def mkRef[A](a: A): Either[Throwable, QuasiRef[FThrowable[Either]#l, A]] = Right(new QuasiRef[FThrowable[Either]#l, A] {
        private final val idRef: QuasiRef[Identity, A]          = id.mkRef[A](a)
        override def get: Either[Throwable, A]                  = Right(idRef.get)
        override def set(a: A): Either[Throwable, Unit]         = Right(idRef.set(a))
        override def update(f: A => A): Either[Throwable, Unit] = Right(idRef.update(f))
      })
    }
  }

  private implicit val quasiIORunner: QuasiIORunner[FThrowable[Either]#l] = new QuasiIORunner[FThrowable[Either]#l] {
    override def run[A](f: => Either[Throwable, A]): A = f.getOrThrow
  }

  private val EitherDefaultModule: DefaultModule[FThrowable[Either]#l] = DefaultModule2(new ModuleDef {
    addImplicit[QuasiIO[FThrowable[Either]#l]]
    addImplicit[QuasiIORunner[FThrowable[Either]#l]]
    addImplicit[QuasiAsync[FThrowable[Either]#l]]
  })
}
