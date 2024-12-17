package io.septimalmind.baboon.translator.csharp

import distage.Subcontext
import io.circe.syntax.*
import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.RuntimeGenOpt
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import io.septimalmind.baboon.translator.{
  BaboonAbstractTranslator,
  OutputFile,
  Sources
}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSBaboonTranslator(defnTranslator: CSDefnTranslator,
                         trans: CSTypeTranslator,
                         handler: Subcontext[IndividualConversionHandler],
                         options: CompilerOptions,
                         codecs: Set[CSCodecTranslator],
                         tools: CSDefnTools,
) extends BaboonAbstractTranslator {

  type Out[T] = Either[NEList[BaboonIssue.TranslationIssue], T]

  override def translate(family: BaboonFamily): Out[Sources] = {
    for {
      translated <- doTranslate(family)
      rt <- sharedRuntime()
      testRuntime <- sharedTestRuntime
      meta <- buildMeta(family)
      toRender = options.runtime match {
        case RuntimeGenOpt.Only    => rt
        case RuntimeGenOpt.With    => rt ++ translated ++ testRuntime
        case RuntimeGenOpt.Without => translated
      }
      rendered = toRender.map { o =>
        val content = renderTree(o)
        (o.path, OutputFile(content, o.isTest))
      }
      unique <- (rendered ++ meta.map {
        case (k, v) => (k, OutputFile(v, isTest = false))
      }).toUniqueMap(c => NEList(BaboonIssue.NonUniqueOutputFiles(c)))
    } yield {
      Sources(unique)
    }
  }

  private def buildMeta(family: BaboonFamily): Out[List[(String, String)]] = {

    val data = family.domains.toSeq.flatMap {
      case (_, lineage) =>
        lineage.versions.toSeq.map {
          case (ver, _) =>
            VersionMeta(lineage.pkg.path.mkString("."), ver.version)
        }

    }
    val meta: OutputMeta = OutputMeta(data.toList)
    val json = meta.asJson.spaces2

    Right(List((s"baboon-meta.json", json)))
  }

  private def renderTree(o: CSDefnTranslator.Output): String = {
    val alwaysAvailable: Set[CSPackageId] =
      if (options.disregardImplicitUsings) {
        Set.empty
      } else {
        Set(csSystemPkg, csCollectionsGenericPkg, csLinqPkg)
      }

    val forcedUses: Set[CSPackageId] =
      if (options.disregardImplicitUsings) {
        Set(csLinqPkg)
      } else {
        Set.empty
      }

    val usedPackages = o.tree.values
      .collect {
        case t: CSValue.CSType => t
      }
      .flatMap { t =>
        if (!t.fq) {
          Seq(t.pkg)
        } else {
          Seq.empty
        }
      }
      .distinct
      .sortBy(_.parts.mkString("."))

    val available = Set(o.pkg)
    val requiredPackages = Set.empty
    val allPackages =
      (requiredPackages ++ usedPackages ++ forcedUses)
        .diff(available ++ alwaysAvailable)

    val imports = allPackages.toSeq
      .map { p =>
        if (p.isStatic) {
          q"using static ${p.parts.mkString(".")};"
        } else {
          q"using ${p.parts.mkString(".")};"
        }

      }
      .join("\n")

    val full =
      Seq(
        Seq(q"#nullable enable"),
        Seq(q"#pragma warning disable 612,618"), // deprecation warnings
        Seq(imports),
        Seq(o.tree)
      ).flatten
        .join("\n\n")

    full.mapRender {
      case t: CSValue.CSTypeName =>
        t.name
      case t: CSValue.CSType if !t.fq =>
        if (o.pkg == t.pkg || !t.pkg.parts.startsWith(o.pkg.parts)) {
          t.name
        } else {
          (t.pkg.parts :+ t.name).mkString(".")
        }

      case t: CSValue.CSType =>
        (t.pkg.parts :+ t.name).mkString(".")
    }
  }

  private def doTranslate(
    family: BaboonFamily
  ): Out[List[CSDefnTranslator.Output]] = {
    // TODO: fix .toSeq.toList

    family.domains.toSeq.toList.map {
      case (_, lineage) =>
        translateLineage(lineage)
    }.biFlatten
  }

  private def translateLineage(
    lineage: BaboonLineage
  ): Out[List[CSDefnTranslator.Output]] = {

    lineage.versions.toSeq.toList.map {
      case (_, domain) =>
        translateDomain(domain, lineage)
    }.biFlatten
  }

  private def translateDomain(domain: Domain,
                              lineage: BaboonLineage,
  ): Out[List[CSDefnTranslator.Output]] = {
    val evo = lineage.evolution
    for {
      defnSources <- domain.defs.meta.nodes.toList.map {
        case (_, defn: DomainMember.User) =>
          defnTranslator.translate(defn, domain, evo)
        case _ => Right(List.empty)
      }.biFlatten
      evosToCurrent = evo.diffs.keySet.filter(_.to == domain.version)
      conversionSources <- if (options.generateConversions) {
        generateConversions(domain, lineage, evosToCurrent, defnSources)
      } else {
        Right(List.empty)
      }
      meta <- if (options.csWriteEvolutionDict) {
        generateMeta(domain, lineage)
      } else {
        Right(List.empty)
      }
    } yield {
      defnSources.map(_.output) ++ conversionSources ++ meta
    }
  }

  private def generateMeta(domain: Domain,
                           lineage: BaboonLineage,
  ): Out[List[CSDefnTranslator.Output]] = {
    val basename = tools.basename(domain, lineage.evolution, options)
    val pkg = trans.toCsPkg(domain.id, domain.version, lineage.evolution)

    val entries = lineage.evolution
      .typesUnchangedSince(domain.version)
      .toList
      .sortBy(_._1.toString)
      .map {
        case (tid, version) =>
          q"""_unmodified.Add("${tid.toString}", "${version.version}");"""
      }

    val metaTree =
      q"""public sealed class BaboonMeta : $iBaboonMeta
         |{
         |    private BaboonMeta()
         |    {
         |        ${entries.join("\n").shift(8).trim}
         |    }
         |
         |    public String UnmodifiedSince(String typeIdString)
         |    {
         |        return _unmodified[typeIdString];
         |    }
         |
         |    private readonly $csDict<$csString, $csString> _unmodified = new ();
         |
         |    internal static $csLazy<BaboonMeta> LazyInstance = new $csLazy<BaboonMeta>(() => new BaboonMeta());
         |
         |    public static BaboonMeta Instance { get { return LazyInstance.Value; } }
         |}""".stripMargin

    val metaSource = Seq(metaTree).join("\n\n")
    val meta = tools.inNs(pkg.parts.toSeq, metaSource)

    val metaOutput = CSDefnTranslator.Output(
      s"$basename/BaboonMeta.cs",
      meta,
      pkg,
      isTest = false
    )

    Right(List(metaOutput))
  }

  private def generateConversions(
    domain: Domain,
    lineage: BaboonLineage,
    toCurrent: Set[EvolutionStep],
    defnOut: List[CSDefnTranslator.OutputExt]
  ): Out[List[CSDefnTranslator.Output]] = {
    val pkg = trans.toCsPkg(domain.id, domain.version, lineage.evolution)

    for {
      convs <- lineage.evolution.rules
        .filter(kv => toCurrent.contains(kv._1))
        .map {
          case (srcVer, rules) =>
            handler
              .provide(pkg)
              .provide(srcVer.from)
              .provide[Domain]("current")(domain)
              .provide[Domain]("source")(lineage.versions(srcVer.from))
              .provide(rules)
              .provide(lineage.evolution)
              .produce()
              .use(_.makeConvs())
        }
        .biFlatten
    } yield {
      val conversionRegs = convs.flatMap(_.reg.iterator.toSeq).toSeq
      val missing = convs.flatMap(_.missing.iterator.toSeq).toSeq

      val converter =
        q"""public interface RequiredConversions {
           |    ${missing.join("\n").shift(4).trim}
           |}
           |
           |public sealed class BaboonConversions : $abstractBaboonConversions
           |{
           |    public BaboonConversions(RequiredConversions requiredConversions)
           |    {
           |        ${conversionRegs.join("\n").shift(8).trim}
           |    }
           |
           |    override public $csList<$csString> VersionsFrom()
           |    {
           |        return new $csList<$csString> { ${toCurrent
             .map(_.from.version)
             .map(v => s"""\"$v\"""")
             .mkString(", ")} };
           |    }
           |
           |    override public $csString VersionTo()
           |    {
           |        return "${domain.version.version}";
           |    }
           |}""".stripMargin

      val codecs =
        q"""public sealed class BaboonCodecs : $abstractBaboonCodecs
           |{
           |    private BaboonCodecs()
           |    {
           |        ${defnOut.map(_.codecReg).join("\n").shift(8).trim}
           |    }
           |
           |    internal static $csLazy<BaboonCodecs> LazyInstance = new $csLazy<BaboonCodecs>(() => new BaboonCodecs());
           |
           |    public static BaboonCodecs Instance { get { return LazyInstance.Value; } }
           |}""".stripMargin

      val basename = tools.basename(domain, lineage.evolution, options)

      val runtimeSource = Seq(converter, codecs).join("\n\n")
      val runtime = tools.inNs(pkg.parts.toSeq, runtimeSource)
      val runtimeOutput = CSDefnTranslator.Output(
        s"$basename/BaboonRuntime.cs",
        runtime,
        pkg,
        isTest = false
      )

      val convertersOutput = convs.map { conv =>
        CSDefnTranslator
          .Output(s"$basename/${conv.fname}", conv.conv, pkg, isTest = false)
      }

      List(runtimeOutput) ++ convertersOutput
    }
  }

  private def sharedRuntime(): Out[List[CSDefnTranslator.Output]] = {
    val codecInterfaceProperties =
      (List(q"public $csString Id { get; }") ++ codecs.toList
        .sortBy(_.getClass.getName)
        .map(_.codecInterfaceProperty())).join("\n")

    val codecImplProperties = codecs.toList
      .sortBy(_.getClass.getName)
      .map(_.codecImplProperty())
      .join("\n")

    val codecImplFields = (List(q"$csString Id") ++ codecs.toList
      .sortBy(_.getClass.getName)
      .map(_.codecImplField())).join(", ")

    val codecGenericImplFields = (List(q"$csString Id") ++ codecs.toList
      .sortBy(_.getClass.getName)
      .map(_.codecGenericImplField())).join(", ")

    val baseCodecsSource =
      q"""public interface IBaboonGenerated {
         |    public $csString BaboonDomainVersion();
         |    public $csString BaboonDomainIdentifier();
         |    public $csString BaboonUnmodifiedSinceVersion();
         |    public $csString BaboonTypeIdentifier();
         |}
         |
         |public interface IBaboonAdtMemberMeta {
         |    public $csString BaboonAdtTypeIdentifier();
         |}
         |
         |public interface IBaboonMeta {
         |    public String UnmodifiedSince(String typeIdString);
         |}
         |
         |public interface IBaboonGeneratedLatest : IBaboonGenerated {}
         |
         |public interface IConversion {
         |    public $csTpe TypeFrom();
         |    public $csString VersionFrom();
         |    public $csTpe TypeTo();
         |    public $csString VersionTo();
         |    public $csString TypeId();
         |}
         |
         |public interface IBaboonGeneratedConversion : IConversion
         |{
         |    public $iBaboonGenerated Convert<TC>(TC? context, $abstractBaboonConversions conversions, $iBaboonGenerated from);
         |}
         |
         |public abstract class AbstractConversion<TFrom, TTo> : IBaboonGeneratedConversion
         |{
         |    public abstract TTo Convert<TCtx>(TCtx? context, $abstractBaboonConversions conversions, TFrom from);
         |
         |    IBaboonGenerated IBaboonGeneratedConversion.Convert<TCtx>(TCtx? context, $abstractBaboonConversions conversions, $iBaboonGenerated from) where TCtx : default
         |    {
         |        if (from is not TFrom fr)
         |        {
         |            throw new Exception($$"Can't use IBaboonGeneratedConversion interface when from is not of type {typeof(TFrom).FullName}");
         |        }
         |        var res = Convert(context, conversions, fr);
         |
         |        if (res is not $iBaboonGenerated bg)
         |        {
         |            throw new $csArgumentException($$"Can't use IBaboonGeneratedConversion interface for non IBaboonGenerated return type To = {typeof(TTo).FullName}");
         |        }
         |        return bg;
         |    }
         |
         |    public $csTpe TypeFrom() {
         |         return typeof(TFrom);
         |    }
         |
         |     public $csTpe TypeTo() {
         |         return typeof(TTo);
         |     }
         |
         |     public abstract $csString VersionFrom();
         |
         |     public abstract $csString VersionTo();
         |
         |     public abstract $csString TypeId();
         |}
         |
         |public interface IBaboonCodecData {
         |    public $csString BaboonDomainVersion();
         |    public $csString BaboonDomainIdentifier();
         |    public $csString BaboonTypeIdentifier();
         |}
         |
         |public interface IBaboonCodec<T> : $iBaboonCodecData {}
         |
         |public interface IBaboonValueCodec<T, TWire> : $iBaboonCodec<T>
         |{
         |    TWire Encode(T instance);
         |    T Decode(TWire wire);
         |}
         |
         |public interface IBaboonJsonCodec<T> : $iBaboonValueCodec<T, $nsJToken> {}
         |
         |public interface IBaboonStreamCodec<T, in TOut, in TIn> : $iBaboonCodec<T>
         |{
         |    void Encode(TOut writer, T instance);
         |    T Decode(TIn wire);
         |}
         |
         |public interface IBaboonBinCodec<T> : $iBaboonStreamCodec<T, $binaryWriter, $binaryReader> {}
         |
         |public interface IBaboonTypeCodecs {
         |${codecInterfaceProperties.shift(4)}
         |}
         |
         |public sealed record BaboonTypeCodecs($codecImplFields) : IBaboonTypeCodecs {
         |${codecImplProperties.shift(4)}
         |};
         |
         |public sealed record BaboonTypeCodecs<T>($codecGenericImplFields) : $iBaboonTypeCodecs {
         |${codecImplProperties.shift(4)}
         |};
         |
         |public abstract class AbstractBaboonCodecs
         |{
         |
         |    private readonly Dictionary<$csString, $iBaboonTypeCodecs> _codecs = new ();
         |
         |    public void Register($iBaboonTypeCodecs impls)
         |    {
         |        _codecs[impls.Id] = impls;
         |    }
         |
         |    public $iBaboonTypeCodecs Find($csString id)
         |    {
         |        return _codecs[id];
         |    }
         |
         |    public bool TryFind($csString id, out $iBaboonTypeCodecs? value)
         |    {
         |        return _codecs.TryGetValue(id, out value);
         |    }
         |}""".stripMargin

    val conversionKeySource =
      q"""public sealed class ConversionKey
         |{
         |    protected bool Equals(ConversionKey other)
         |    {
         |        return TypeFrom.Equals(other.TypeFrom) && TypeTo.Equals(other.TypeTo);
         |    }
         |
         |    public override bool Equals(object? obj)
         |    {
         |        if (ReferenceEquals(null, obj)) return false;
         |        if (ReferenceEquals(this, obj)) return true;
         |        if (obj.GetType() != this.GetType()) return false;
         |        return Equals((ConversionKey)obj);
         |    }
         |
         |    public override int GetHashCode()
         |    {
         |        return HashCode.Combine(TypeFrom, TypeTo);
         |    }
         |
         |    public ConversionKey($csTpe typeFrom, $csTpe typeTo)
         |    {
         |        TypeFrom = typeFrom;
         |        TypeTo = typeTo;
         |    }
         |
         |    public $csTpe TypeFrom { get; }
         |    public $csTpe TypeTo {get; }
         |}""".stripMargin

    val conversionsSource =
      q"""public abstract class AbstractBaboonConversions
         |{
         |    private readonly $csDict<ConversionKey, IConversion> _convs = new ();
         |    private readonly $csDict<Type, List<IConversion>> _convsWild = new ();
         |
         |    public abstract $csList<String> VersionsFrom();
         |
         |    public abstract $csString VersionTo();
         |
         |    public $csList<IConversion> AllConversions()
         |    {
         |        return _convs.Values.ToList();
         |    }
         |
         |    public void Register(IConversion conversion)
         |    {
         |        var fromType = conversion.TypeFrom();
         |        var key = new ConversionKey(fromType, conversion.TypeTo());
         |        var wild = _convsWild.TryGetValue(fromType, out var v) ? v : new $csList<IConversion>();
         |        wild.Add(conversion);
         |        _convs[key] = conversion;
         |        _convsWild[fromType] = wild;
         |    }
         |
         |    public void Register<TFrom, TTo>(AbstractConversion<TFrom, TTo> conversion)
         |    {
         |        var tFrom = typeof(TFrom);
         |        var tTo = typeof(TTo);
         |        var key = new ConversionKey(tFrom, tTo);
         |        var wild = _convsWild.TryGetValue(tFrom, out var v) ? v : new List<IConversion>();
         |        wild.Add(conversion);
         |        _convs[key] = conversion;
         |        _convsWild[tFrom] = wild;
         |    }
         |
         |    public IBaboonGenerated ConvertWithContext<T>(T? c, IBaboonGenerated from, IConversion conversion)
         |    {
         |        var tconv = (IBaboonGeneratedConversion)conversion;
         |        return tconv.Convert<T>(c, this, from);
         |    }
         |
         |    public IBaboonGenerated Convert(IBaboonGenerated from, IConversion conversion)
         |    {
         |        var tconv = (IBaboonGeneratedConversion)conversion;
         |        return tconv.Convert<Object>(null, this, from);
         |    }
         |
         |    public IReadOnlyList<IConversion> FindConversions(IBaboonGenerated value)
         |    {
         |        return !_convsWild.TryGetValue(value.GetType(), out var res) ? new List<IConversion>() : res;
         |    }
         |
         |    public TTo ConvertWithContext<T, TFrom, TTo>(T? c, TFrom from)
         |    {
         |        var tFrom = typeof(TFrom);
         |        var tTo = typeof(TTo);
         |
         |        if (from is TTo direct)
         |        {
         |            return direct;
         |        }
         |        var key = new ConversionKey(tFrom, tTo);
         |
         |        var conv = _convs[key];
         |        var tconv = ((AbstractConversion<TFrom, TTo>)conv);
         |        return tconv.Convert(c, this, from);
         |    }
         |
         |    public TTo Convert<TFrom, TTo>(TFrom from)
         |        where TFrom : IBaboonGenerated
         |        where TTo : IBaboonGenerated
         |    {
         |        return ConvertWithContext<Object, TFrom, TTo>(null, from);
         |    }
         |
         |}""".stripMargin

    val baseToolsSource =
      q"""public static class BaboonTools {
         |    public static T? ReadNullableValueType<T>(Boolean ifNot, Func<T> thenReturn) where T: struct
         |    {
         |        if (ifNot) return null;
         |        return thenReturn();
         |    }
         |
         |    public static T? ReadNullableValueType<T>($nsJToken? token, Func<$nsJToken, T> readValue) where T: struct
         |    {
         |        if (token == null || token.Type == $nsJTokenType.Null) return null;
         |        return readValue(token);
         |    }
         |
         |    public static T? ReadNullableReferentialType<T>($nsJToken? token, Func<$nsJToken, T> readValue) where T: class
         |    {
         |        if (token == null || token.Type == $nsJTokenType.Null) return null;
         |        return readValue(token);
         |    }
         |}
         |""".stripMargin

    val rpDateTimeSource =
      q"""/** Reduced to milliseconds precision DateTime */
         |[$nsJsonConverter(typeof(JsonConverter))]
         |public readonly struct RpDateTime : $csIComparable, $csIComparable<RpDateTime>, $csIEquatable<RpDateTime>
         |{
         |    public readonly $csDateTime DateTime;
         |
         |    public RpDateTime(int year, int month, int day, int hour, int minute, int second, int millisecond, DateTimeKind kind)
         |    {
         |        DateTime = new $csDateTime(year, month, day, hour, minute, second, millisecond, kind);
         |    }
         |
         |    public RpDateTime(long ticks, $csDateTimeKind kind)
         |    {
         |        DateTime = $baboonTimeFormats.TruncateToMilliseconds(ticks, kind);
         |    }
         |
         |    public RpDateTime(DateTime dateTime)
         |    {
         |        DateTime = $baboonTimeFormats.TruncateToMilliseconds(dateTime);
         |    }
         |
         |    public override int GetHashCode()
         |    {
         |        return DateTime.GetHashCode();
         |    }
         |
         |    public override bool Equals(object? obj)
         |    {
         |        if (obj is not RpDateTime other) return false;
         |        return other.DateTime.ToUniversalTime() == DateTime.ToUniversalTime();
         |    }
         |
         |    public bool Equals(RpDateTime other)
         |    {
         |        if (other == null!) return false;
         |        return other.DateTime.ToUniversalTime() == DateTime.ToUniversalTime();
         |    }
         |
         |    public override string ToString() => BaboonDateTimeFormats.ToString(this);
         |    public string ToString(string format) => DateTime.ToString(format);
         |
         |    public long Ticks => DateTime.Ticks;
         |    public DateTimeKind Kind => DateTime.Kind;
         |    public RpDateTime ToUniversalTime() => new RpDateTime(DateTime.ToUniversalTime());
         |    public RpDateTime ToLocalTime() => new RpDateTime(DateTime.ToLocalTime());
         |    public RpDateTime LocalDate => new RpDateTime(DateTime.ToLocalTime().Date);
         |    public RpDateTime Date => new RpDateTime(DateTime.Date);
         |    public TimeSpan GetUtcOffset() => TimeZoneInfo.Local.GetUtcOffset(DateTime);
         |    public TimeSpan Subtract(RpDateTime right) => DateTime.ToUniversalTime().Subtract(right.DateTime.ToUniversalTime());
         |    public RpDateTime Subtract(TimeSpan span) => new RpDateTime(DateTime.Subtract(span));
         |    public RpDateTime Add(TimeSpan value) => new RpDateTime(DateTime.Add(value));
         |    public RpDateTime AddTicks(long value) => new RpDateTime(DateTime.AddTicks(value));
         |    public RpDateTime AddMilliseconds(double value) => new RpDateTime(DateTime.AddMilliseconds(value));
         |    public RpDateTime AddSeconds(double value) => new RpDateTime(DateTime.AddSeconds(value));
         |    public RpDateTime AddMinutes(double value) => new RpDateTime(DateTime.AddMinutes(value));
         |    public RpDateTime AddHours(double value) => new RpDateTime(DateTime.AddHours(value));
         |    public RpDateTime AddDays(double value) => new RpDateTime(DateTime.AddDays(value));
         |    public RpDateTime AddMonths(int value) => new RpDateTime(DateTime.AddMonths(value));
         |    public RpDateTime AddYears(int value) => new RpDateTime(DateTime.AddYears(value));
         |
         |    public int DiffInFullHours(RpDateTime other) => (int) (this - other).TotalHours;
         |    public int DiffInFullDays(RpDateTime other) => (int) (this - other).TotalDays;
         |    public int DiffInFullWeeks(RpDateTime other) => DiffInFullDays(other) / 7;
         |    public int DiffInFullMonths(RpDateTime other) => Date == other.Date ? 0 : (Year - other.Year) * 12 + Month - other.Month + GetMonthsDiffByDays(other);
         |    public int DiffInFullYears(RpDateTime other) => DiffInFullMonths(other) / 12;
         |
         |    public static RpDateTime Now => new RpDateTime($csDateTime.Now);
         |    public static RpDateTime UtcNow => new RpDateTime($csDateTime.UtcNow);
         |    public static RpDateTime Epoch => new RpDateTime(1970, 1, 1, 0, 0, 0, 0, $csDateTimeKind.Utc);
         |    public static RpDateTime MinValue => new RpDateTime($csDateTime.MinValue);
         |    public static RpDateTime MaxValue => new RpDateTime($csDateTime.MaxValue);
         |
         |    public int Year => DateTime.Year;
         |    public int Month => DateTime.Month;
         |    public int Day => DateTime.Day;
         |    public int DayOfYear => DateTime.DayOfYear;
         |
         |    public $csTimeSpan TimeOfDay => DateTime.TimeOfDay;
         |    public $csDayOfWeek DayOfWeek => DateTime.DayOfWeek;
         |
         |    public int CompareTo(object obj)
         |    {
         |        if (obj is RpDateTime dt) return DateTime.CompareTo(dt.DateTime);
         |        throw new ArgumentException("Argument is not RpDateTime.");
         |    }
         |
         |    public int CompareTo(RpDateTime other)
         |    {
         |        return other == null! ? DateTime.CompareTo(null) : DateTime.CompareTo(other.DateTime);
         |    }
         |
         |    public static RpDateTime operator -(RpDateTime left, $csTimeSpan delta)
         |    {
         |        return new RpDateTime(left.DateTime - delta);
         |    }
         |
         |    public static RpDateTime operator +(RpDateTime left, $csTimeSpan delta)
         |    {
         |        return new RpDateTime(left.DateTime + delta);
         |    }
         |
         |    public static bool operator ==(RpDateTime left, RpDateTime right)
         |    {
         |        return left.Equals(right);
         |    }
         |
         |    public static bool operator !=(RpDateTime left, RpDateTime right)
         |    {
         |        return !(left == right);
         |    }
         |
         |    public static TimeSpan operator -(RpDateTime left, RpDateTime right)
         |    {
         |        return left.DateTime.ToUniversalTime() - right.DateTime.ToUniversalTime();
         |    }
         |
         |    public static bool operator >(RpDateTime left, RpDateTime right)
         |    {
         |        return left.DateTime.ToUniversalTime() > right.DateTime.ToUniversalTime();
         |    }
         |
         |    public static bool operator <(RpDateTime left, RpDateTime right)
         |    {
         |        return left.DateTime.ToUniversalTime() < right.DateTime.ToUniversalTime();
         |    }
         |
         |    public static bool operator <=(RpDateTime left, RpDateTime right)
         |    {
         |        return left.DateTime.ToUniversalTime() <= right.DateTime.ToUniversalTime();
         |    }
         |
         |    public static bool operator >=(RpDateTime left, RpDateTime right)
         |    {
         |        return left.DateTime.ToUniversalTime() >= right.DateTime.ToUniversalTime();
         |    }
         |
         |    public static implicit operator RpDateTime($csDateTime dt) => new(dt);
         |    public static implicit operator $csDateTime(RpDateTime dt) => dt.DateTime;
         |
         |    public static RpDateTime Parse($csString dt) => $baboonTimeFormats.FromString(dt);
         |
         |    private int GetMonthsDiffByDays(RpDateTime other)
         |    {
         |        var thisDaysInMonth = DateTime.DaysInMonth(Year, Month);
         |        var otherDaysInMonth = DateTime.DaysInMonth(other.Year, other.Month);
         |
         |        var thisDay = thisDaysInMonth < otherDaysInMonth ? Day : thisDaysInMonth == Day ? otherDaysInMonth : Day;
         |        var otherDay = otherDaysInMonth < thisDaysInMonth ? other.Day : otherDaysInMonth == other.Day ? thisDaysInMonth : other.Day;
         |
         |        if (this < other)
         |        {
         |            if (thisDay > otherDay) return 1;
         |            if (thisDay == otherDay && TimeOfDay > other.TimeOfDay) return 1;
         |        }
         |        else
         |        {
         |            if (thisDay < otherDay) return -1;
         |            if (thisDay == otherDay && TimeOfDay < other.TimeOfDay) return -1;
         |        }
         |
         |        return 0;
         |    }
         |
         |    private sealed class JsonConverter : $nsJsonConverter<RpDateTime>
         |    {
         |        public override void WriteJson($nsJsonWriter writer, RpDateTime value, $nsJsonSerializer serializer)
         |        {
         |            writer.WriteValue($baboonTimeFormats.ToString(value));
         |        }
         |
         |        public override RpDateTime ReadJson($nsJsonReader reader, $csTpe objectType, RpDateTime existingValue, bool hasExistingValue, $nsJsonSerializer serializer)
         |        {
         |            return $baboonTimeFormats.FromString((string) reader.Value!);
         |        }
         |    }
         |}""".stripMargin

    val timeFormatsSource =
      q"""public static class BaboonDateTimeFormats
         |{
         |    public static readonly String TslDefault = "yyyy-MM-ddTHH:mm:ss.fff";
         |
         |    public static readonly String[] Tsl = new string[]
         |    {
         |        "yyyy-MM-ddTHH:mm:ss",
         |        "yyyy-MM-ddTHH:mm:ss.f",
         |        "yyyy-MM-ddTHH:mm:ss.ff",
         |        "yyyy-MM-ddTHH:mm:ss.fff",
         |        "yyyy-MM-ddTHH:mm:ss.ffff",
         |        "yyyy-MM-ddTHH:mm:ss.fffff",
         |        "yyyy-MM-ddTHH:mm:ss.ffffff",
         |        "yyyy-MM-ddTHH:mm:ss.fffffff",
         |        "yyyy-MM-ddTHH:mm:ss.ffffffff",
         |        "yyyy-MM-ddTHH:mm:ss.fffffffff"
         |    };
         |
         |    public static readonly String TszDefault = "yyyy-MM-ddTHH:mm:ss.fffzzz";
         |
         |    public static readonly String[] Tsz = new string[]
         |    {
         |        "yyyy-MM-ddTHH:mm:ssZ",
         |        "yyyy-MM-ddTHH:mm:ss.fZ",
         |        "yyyy-MM-ddTHH:mm:ss.ffZ",
         |        "yyyy-MM-ddTHH:mm:ss.fffZ",
         |        "yyyy-MM-ddTHH:mm:ss.ffffZ",
         |        "yyyy-MM-ddTHH:mm:ss.fffffZ",
         |        "yyyy-MM-ddTHH:mm:ss.ffffffZ",
         |        "yyyy-MM-ddTHH:mm:ss.fffffffZ",
         |        "yyyy-MM-ddTHH:mm:ss.ffffffffZ",
         |        "yyyy-MM-ddTHH:mm:ss.fffffffffZ",
         |        "yyyy-MM-ddTHH:mm:sszzz",
         |        "yyyy-MM-ddTHH:mm:ss.fzzz",
         |        "yyyy-MM-ddTHH:mm:ss.ffzzz",
         |        "yyyy-MM-ddTHH:mm:ss.fffzzz",
         |        "yyyy-MM-ddTHH:mm:ss.ffffzzz",
         |        "yyyy-MM-ddTHH:mm:ss.fffffzzz",
         |        "yyyy-MM-ddTHH:mm:ss.ffffffzzz",
         |        "yyyy-MM-ddTHH:mm:ss.fffffffzzz",
         |        "yyyy-MM-ddTHH:mm:ss.ffffffffzzz",
         |        "yyyy-MM-ddTHH:mm:ss.fffffffffzzz"
         |    };
         |
         |    public static readonly String TsuDefault = "yyyy-MM-ddTHH:mm:ss.fffZ";
         |    public static readonly String[] Tsu = Tsz;
         |
         |    public static String ToString($csDateTime dt)
         |    {
         |        return dt.ToString(dt.Kind == $csDateTimeKind.Utc ? TsuDefault : TszDefault, $csInvariantCulture.InvariantCulture);
         |    }
         |
         |    public static String ToString(RpDateTime dt)
         |    {
         |        return dt.DateTime.ToString(dt.DateTime.Kind == $csDateTimeKind.Utc ? TsuDefault : TszDefault, $csInvariantCulture.InvariantCulture);
         |    }
         |
         |    public static RpDateTime FromString(String dt)
         |    {
         |        return $csDateTime.ParseExact(dt, Tsz, $csInvariantCulture.InvariantCulture, $csDateTimeStyles.None);
         |    }
         |
         |    public static $csDateTime TruncateToMilliseconds(long ticks, $csDateTimeKind kind)
         |    {
         |        return new $csDateTime(ticks - (ticks % TimeSpan.TicksPerMillisecond), kind);
         |    }
         |
         |    public static $csDateTime TruncateToMilliseconds($csDateTime dateTime)
         |    {
         |        return TruncateToMilliseconds(dateTime.Ticks, dateTime.Kind);
         |    }
         |}
         |""".stripMargin

    val sharedRuntimeSource = Seq(
      baseCodecsSource,
      baseToolsSource,
      conversionKeySource,
      conversionsSource
    ).join("\n\n")
    val sharedRuntime = tools.inNs(
      CSBaboonTranslator.baboonRtPkg.parts.toSeq,
      sharedRuntimeSource
    )
    val sharedOutput = CSDefnTranslator.Output(
      s"BaboonRuntimeShared.cs",
      sharedRuntime,
      CSBaboonTranslator.baboonRtPkg,
      isTest = false
    )

    val timeSource = Seq(rpDateTimeSource, timeFormatsSource).join("\n\n")
    val time =
      tools.inNs(CSBaboonTranslator.baboonTimePkg.parts.toSeq, timeSource)
    val timeOutput = CSDefnTranslator.Output(
      s"BaboonTime.cs",
      time,
      CSBaboonTranslator.baboonTimePkg,
      isTest = false
    )

    Right(List(sharedOutput, timeOutput))
  }

  private def sharedTestRuntime: Out[List[CSDefnTranslator.Output]] = {
    val sharedTestRuntime =
      q"""// RandomValuesGenerator
        |public static class RVG
        |{
        |    private static readonly $csRandom Rnd = new $csRandom();
        |    private const $csString Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
        |
        |    public static $csBoolean NextBoolean()
        |    {
        |        return Rnd.Next(0, 2) == 1;
        |    }
        |
        |    public static $csSByte NextSByte()
        |    {
        |        return ($csSByte)Rnd.Next($csSByte.MinValue, $csSByte.MaxValue);
        |    }
        |
        |    public static $csInt16 NextInt16()
        |    {
        |        return ($csInt16)Rnd.Next($csInt16.MinValue, $csInt16.MaxValue);
        |    }
        |
        |    public static $csInt32 NextInt32()
        |    {
        |        return Rnd.Next($csInt32.MinValue, $csInt32.MaxValue);
        |    }
        |
        |    public static $csInt64 NextInt64()
        |    {
        |        return Rnd.NextInt64($csInt64.MinValue, $csInt64.MaxValue);
        |    }
        |
        |    public static $csByte NextByte()
        |    {
        |        return ($csByte)Rnd.Next(0, $csByte.MaxValue);
        |    }
        |
        |    public static $csUInt16 NextUInt16()
        |    {
        |        return ($csUInt16)Rnd.Next(0, $csUInt16.MaxValue);
        |    }
        |
        |    public static $csUInt32 NextUInt32()
        |    {
        |        return ($csUInt32)Rnd.Next(0, $csInt32.MaxValue);
        |    }
        |
        |    public static $csUInt64 NextUInt64()
        |    {
        |        return ($csUInt64)Rnd.Next(0, $csInt32.MaxValue);
        |    }
        |
        |    public static $csSingle NextSingle()
        |    {
        |        return Rnd.NextSingle();
        |    }
        |
        |    public static $csDouble NextDouble()
        |    {
        |        return Rnd.NextDouble();
        |    }
        |
        |    public static $csDecimal NextDecimal()
        |    {
        |        return ($csDecimal)Rnd.NextDouble();
        |    }
        |
        |    public static $csString NextString()
        |    {
        |        var length = Rnd.Next(0, 21);
        |
        |        var stringBuilder = new $csStringBuilder(length);
        |
        |        for (var i = 0; i < length; i++)
        |        {
        |            var randomChar = Chars[Rnd.Next(Chars.Length)];
        |            stringBuilder.Append(randomChar);
        |        }
        |
        |        return stringBuilder.ToString();
        |    }
        |
        |    public static $rpDateTime NextRpDateTime()
        |    {
        |        var minTicks = $csDateTime.MinValue.Ticks;
        |        var maxTicks = $csDateTime.MaxValue.Ticks;
        |
        |        var randomTicks = ($csInt64)(Rnd.NextDouble() * (maxTicks - minTicks)) + minTicks;
        |
        |        return new $rpDateTime(new $csDateTime(randomTicks));
        |    }
        |
        |    public static $csGuid NextGuid()
        |    {
        |        return $csGuid.NewGuid();
        |    }
        |
        |    public static T NextRandomEnum<T>() where T : $csEnum
        |    {
        |        var values = $csEnum.GetValues(typeof(T));
        |        return (T)values.GetValue(Rnd.Next(values.Length))!;
        |    }
        |
        |    public static $csImmutableDictionary<TK, TV> CreateImmutableDictionary<TK, TV>($csList<KeyValuePair<TK, TV>> values) where TK : notnull
        |    {
        |        var map = new Dictionary<TK, TV>(values.Count);
        |        values.ForEach(pair => map.TryAdd(pair.Key, pair.Value));
        |
        |        return map.ToImmutableDictionary();
        |    }
        |}
        |""".stripMargin

    Right(
      List(
        CSDefnTranslator.Output(
          "BaboonTestRuntimeShared.cs",
          tools.inNs(
            CSBaboonTranslator.baboonTestRtPkg.parts.toSeq,
            sharedTestRuntime
          ),
          CSBaboonTranslator.baboonTestRtPkg,
          isTest = true,
        )
      )
    )
  }

}

object CSBaboonTranslator {
  case class RenderedConversion(fname: String,
                                conv: TextTree[CSValue],
                                reg: Option[TextTree[CSValue]],
                                missing: Option[TextTree[CSValue]],
  )

  // Baboon packages
  val baboonRtPkg: CSPackageId = CSPackageId(
    NEList("Baboon", "Runtime", "Shared")
  )
  val baboonTestRtPkg: CSPackageId = CSPackageId(
    NEList("Baboon", "Test", "Runtime", "Shared")
  )
  val baboonTimePkg: CSPackageId = CSPackageId(NEList("Baboon", "Time"))

  // System packages
  val csSystemPkg: CSPackageId = CSPackageId(NEList("System"))
  val csGlobalizationPkg: CSPackageId = CSPackageId(
    NEList("System", "Globalization")
  )
  val csCollectionsGenericPkg: CSPackageId = CSPackageId(
    NEList("System", "Collections", "Generic")
  )
  val csCollectionsImmutablePkg: CSPackageId = CSPackageId(
    NEList("System", "Collections", "Immutable")
  )
  val csLinqPkg: CSPackageId = CSPackageId(NEList("System", "Linq"))
  val csIoPkg: CSPackageId = CSPackageId(NEList("System", "IO"))
  val csTextPkg: CSPackageId = CSPackageId(NEList("System.Text"))
  val csDiagnosticsPkg: CSPackageId = CSPackageId(
    NEList("System", "Diagnostics")
  )

  // Newtonsoft packages
  val nsPkg: CSPackageId = CSPackageId(NEList("Newtonsoft", "Json"))
  val nsLinqPkg: CSPackageId = CSPackageId(NEList("Newtonsoft", "Json", "Linq"))
  val nunitPkg: CSPackageId = CSPackageId(NEList("NUnit", "Framework"))

  // Nunit types
  val nunitTestFixture: CSType =
    CSType(nunitPkg, "TestFixture", fq = false)
  val nunitOneTimeSetUp: CSType =
    CSType(nunitPkg, "OneTimeSetUp", fq = false)
  val testValuesGenerator: CSType =
    CSType(baboonTestRtPkg, "RVG", fq = false)

  // Baboon conversions' types
  val abstractConversion: CSType =
    CSType(baboonRtPkg, "AbstractConversion", fq = false)
  val abstractBaboonConversions: CSType =
    CSType(baboonRtPkg, "AbstractBaboonConversions", fq = false)
  val iBaboonGenerated: CSType =
    CSType(baboonRtPkg, "IBaboonGenerated", fq = false)
  val iBaboonAdtMemberMeta: CSType =
    CSType(baboonRtPkg, "IBaboonAdtMemberMeta", fq = false)
  val iBaboonGeneratedLatest: CSType =
    CSType(baboonRtPkg, "IBaboonGeneratedLatest", fq = false)
  val BaboonTools: CSType =
    CSType(baboonRtPkg, "BaboonTools", fq = false)

  // Baboon codec types
  val iBaboonCodecData: CSType =
    CSType(baboonRtPkg, "IBaboonCodecData", fq = false)
  val iBaboonCodec: CSType =
    CSType(baboonRtPkg, "IBaboonCodec", fq = false)
  val iBaboonValueCodec: CSType =
    CSType(baboonRtPkg, "IBaboonValueCodec", fq = false)
  val iBaboonStreamCodec: CSType =
    CSType(baboonRtPkg, "IBaboonStreamCodec", fq = false)

  val iBaboonJsonCodec: CSType =
    CSType(baboonRtPkg, "IBaboonJsonCodec", fq = false)
  val iBaboonBinCodec: CSType =
    CSType(baboonRtPkg, "IBaboonBinCodec", fq = false)
  val iBaboonTypeCodecs: CSType =
    CSType(baboonRtPkg, "IBaboonTypeCodecs", fq = false)
  val baboonTypeCodecs: CSType =
    CSType(baboonRtPkg, "BaboonTypeCodecs", fq = false)
  val abstractBaboonCodecs: CSType =
    CSType(baboonRtPkg, "AbstractBaboonCodecs", fq = false)

  val baboonTimeFormats: CSType =
    CSType(baboonTimePkg, "BaboonDateTimeFormats", fq = false)

  val iBaboonMeta: CSType =
    CSType(baboonRtPkg, "IBaboonMeta", fq = false)

  // Baboon type
  val rpDateTime: CSType =
    CSType(baboonTimePkg, "RpDateTime", fq = false)

  // Newtonsoft types
  val nsJsonWriter: CSType =
    CSType(nsPkg, "JsonWriter", fq = false)
  val nsJsonReader: CSType =
    CSType(nsPkg, "JsonReader", fq = false)
  val nsJsonSerializer: CSType =
    CSType(nsPkg, "JsonSerializer", fq = false)
  val nsJsonConverter: CSType =
    CSType(nsPkg, "JsonConverter", fq = false)
  val nsFormatting: CSType =
    CSType(nsPkg, "Formatting", fq = false)
  val nsJToken: CSType =
    CSType(nsLinqPkg, "JToken", fq = false)
  val nsJValue: CSType =
    CSType(nsLinqPkg, "JValue", fq = false)
  val nsJArray: CSType =
    CSType(nsLinqPkg, "JArray", fq = false)
  val nsJObject: CSType =
    CSType(nsLinqPkg, "JObject", fq = false)
  val nsJProperty: CSType =
    CSType(nsLinqPkg, "JProperty", fq = false)
  val nsJTokenType: CSType =
    CSType(nsLinqPkg, "JTokenType", fq = false)

  val binaryReader: CSType =
    CSType(csIoPkg, "BinaryReader", fq = false)
  val binaryWriter: CSType =
    CSType(csIoPkg, "BinaryWriter", fq = false)
  val memoryStream: CSType =
    CSType(csIoPkg, "MemoryStream", fq = false)

  // C# types
  val csString: CSType =
    CSType(csSystemPkg, "String", fq = false)
  val csGuid: CSType =
    CSType(csSystemPkg, "Guid", fq = false)
  val csBoolean: CSType =
    CSType(csSystemPkg, "Boolean", fq = false)
  val csStringBuilder: CSType =
    CSType(csTextPkg, "StringBuilder", fq = false)

  val csSByte: CSType =
    CSType(csSystemPkg, "sbyte", fq = false)
  val csInt16: CSType =
    CSType(csSystemPkg, "Int16", fq = false)
  val csInt32: CSType =
    CSType(csSystemPkg, "Int32", fq = false)
  val csInt64: CSType =
    CSType(csSystemPkg, "Int64", fq = false)

  val csByte: CSType =
    CSType(csSystemPkg, "byte", fq = false)
  val csUInt16: CSType =
    CSType(csSystemPkg, "UInt16", fq = false)
  val csUInt32: CSType =
    CSType(csSystemPkg, "UInt32", fq = false)
  val csUInt64: CSType =
    CSType(csSystemPkg, "UInt64", fq = false)

  val csSingle: CSType =
    CSType(csSystemPkg, "Single", fq = false)
  val csDouble: CSType =
    CSType(csSystemPkg, "Double", fq = false)
  val csDecimal: CSType =
    CSType(csSystemPkg, "Decimal", fq = false)

  val csTpe: CSType =
    CSType(csSystemPkg, "Type", fq = false)
  val csLazy: CSType =
    CSType(csSystemPkg, "Lazy", fq = false)
  val csList: CSType =
    CSType(csCollectionsGenericPkg, "List", fq = false)
  val csDict: CSType =
    CSType(csCollectionsGenericPkg, "Dictionary", fq = false)
  val csEnum: CSType =
    CSType(csSystemPkg, "Enum", fq = false)
  val csDateTime: CSType =
    CSType(csSystemPkg, "DateTime", fq = false)
  val csTimeSpan: CSType =
    CSType(csSystemPkg, "TimeSpan", fq = false)
  val csDayOfWeek: CSType =
    CSType(csSystemPkg, "DayOfWeek", fq = false)
  val csArgumentException: CSType =
    CSType(csSystemPkg, "ArgumentException", fq = false)
  val csEnumerable: CSType =
    CSType(csLinqPkg, "Enumerable", fq = false)
  val csRandom: CSType =
    CSType(csSystemPkg, "Random", fq = false)
  val csIComparable: CSType =
    CSType(csSystemPkg, "IComparable", fq = false)
  val csIEquatable: CSType =
    CSType(csSystemPkg, "IEquatable", fq = false)

  val csImmutableDictionary: CSType =
    CSType(csCollectionsImmutablePkg, "ImmutableDictionary", fq = false)
  val csKeyValuePair: CSType =
    CSType(csCollectionsGenericPkg, "KeyValuePair", fq = false)

  val csInvariantCulture: CSType =
    CSType(csGlobalizationPkg, "CultureInfo", fq = false)
  val csDateTimeStyles: CSType =
    CSType(csGlobalizationPkg, "DateTimeStyles", fq = false)
  val csDateTimeKind: CSType =
    CSType(csSystemPkg, "DateTimeKind", fq = false)
  val csTimeZoneInfo: CSType =
    CSType(csSystemPkg, "TimeZoneInfo", fq = false)

  val debug: CSType =
    CSType(csDiagnosticsPkg, "Debug", fq = false)
}
