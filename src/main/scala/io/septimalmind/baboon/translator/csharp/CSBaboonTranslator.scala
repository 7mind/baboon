package io.septimalmind.baboon.translator.csharp

import distage.Subcontext
import io.circe.syntax.*
import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.RuntimeGenOpt
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSBaboonTranslator(
                          defnTranslator: CSDefnTranslator,
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
        case RuntimeGenOpt.Only => rt
        case RuntimeGenOpt.With => rt ++ translated ++ testRuntime
        case RuntimeGenOpt.Without => translated
      }
      rendered = toRender.map { o =>
        val content = renderTree(o)
        (o.path, OutputFile(content, o.isTest))
      }
      unique <- (rendered ++ meta.map { case (k, v) => (k, OutputFile(v, isTest = false)) }).toUniqueMap(
        c => NEList(BaboonIssue.NonUniqueOutputFiles(c))
      )
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
        q"using ${p.parts.mkString(".")};"
      }
      .join("\n")

    val full =
      Seq(
        Seq(q"#nullable enable", q"#pragma warning disable 612,618"),
        Seq(imports),
        Seq(o.tree)
      ).flatten
        .join("\n\n")

    full.mapRender {
      case t: CSValue.CSType if !t.fq =>
        t.name
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
        //val isLatest =
        translateDomain(domain, lineage)
    }.biFlatten
  }

  private def translateDomain(
                               domain: Domain,
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
    } yield {
      defnSources.map(_.output) ++ conversionSources
    }
  }

  private def sharedRuntime(): Out[List[CSDefnTranslator.Output]] = {
    val metaFields = (List(q"String Id") ++ codecs.toList
      .sortBy(_.getClass.getName)
      .map(_.metaField())).join(", ")

    val baseCodecsSource =
      q"""public interface IBaboonGenerated {
         |    public $csString BaboonDomainVersion();
         |    public $csString BaboonDomainIdentifier();
         |    public $csString BaboonTypeIdentifier();
         |}
         |
         |public interface IBaboonGeneratedLatest : IBaboonGenerated {}
         |
         |public interface IConversion {
         |    public $csTpe TypeFrom();
         |    public $csTpe TypeTo();
         |}
         |
         |public interface IBaboonGeneratedConversion : IConversion
         |{
         |    public $iBaboonGenerated Convert<TC>(TC? context, $abstractBaboonConversions conversions, $iBaboonGenerated from);
         |}
         |
         |public interface IDynamicConversion<out TTo> : IConversion
         |{
         |     public TTo Convert<T>(T? context, $abstractBaboonConversions conversions, dynamic from);
         |}
         |
         |public abstract class AbstractConversion<TFrom, TTo> : IDynamicConversion<TTo>, IBaboonGeneratedConversion
         |{
         |    public abstract TTo Convert<TCtx>(TCtx? context, $abstractBaboonConversions conversions, TFrom from);
         |
         |    public TTo Convert<TCtx>(TCtx? context, $abstractBaboonConversions conversions, dynamic from)
         |    {
         |        return Convert<TCtx>(context, conversions, (TFrom)from);
         |    }
         |
         |    IBaboonGenerated IBaboonGeneratedConversion.Convert<TCtx>(TCtx? context, $abstractBaboonConversions conversions, $iBaboonGenerated from) where TCtx : default
         |    {
         |        if (from is not TFrom fr)
         |        {
         |            throw new Exception("Can't use IBaboonGeneratedConversion interface when from is not of type {typeof(To).FullName}");
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
         |public record BaboonTypeCodecs($metaFields);
         |
         |public abstract class AbstractBaboonCodecs
         |{
         |
         |    private readonly Dictionary<$csString, $baboonTypeCodecs> _codecs = new ();
         |
         |    public void Register($baboonTypeCodecs impls)
         |    {
         |        _codecs[impls.Id] = impls;
         |    }
         |
         |    public $baboonTypeCodecs Find($csString id)
         |    {
         |        return _codecs[id];
         |    }
         |
         |    public bool TryFind($csString id, out $baboonTypeCodecs? value)
         |    {
         |        return _codecs.TryGetValue(id, out value);
         |    }
         |}""".stripMargin

    val conversionKeySource =
      q"""public class ConversionKey
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
         |    public TTo ConvertWithContextDynamic<T, TTo>(T? c, Type tFrom, dynamic from)
         |    {
         |        var tTo = typeof(TTo);
         |
         |        if (from is TTo direct)
         |        {
         |            return direct;
         |        }
         |        var key = new ConversionKey(tFrom, tTo);
         |
         |        var conv = _convs[key];
         |        var tconv = ((IDynamicConversion<TTo>)conv);
         |        return tconv.Convert(c, this, from);
         |    }
         |
         |    public TTo ConvertDynamic<TTo>(dynamic from)
         |        where TTo : IBaboonGenerated
         |    {
         |        return ConvertWithContextDynamic<Object, TTo>(null, from.GetType(), from);
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
      q"""public class BaboonTools {
         |    public static T? ReadNullableValue<T>(Boolean ifNot, Func<T> thenReturn) where T: struct
         |    {
         |        if (ifNot) return null;
         |        return thenReturn();
         |    }
         |
         |    public static T? ReadNullableValue<T>($nsJToken? token, Func<$nsJToken, T> readValue) where T: struct
         |    {
         |        if (token == null || token.Type == $nsJTokenType.Null) return null;
         |        return readValue(token);
         |    }
         |
         |    public static T? ReadValue<T>($nsJToken? token, Func<$nsJToken, T> readValue) where T: class
         |    {
         |        if (token == null || token.Type == $nsJTokenType.Null) return null;
         |        return readValue(token);
         |    }
         |}
         |""".stripMargin

    val rpDateTimeSource =
      q"""/** Reduced to milliseconds precision DateTime */
         |[$nsJsonConverter(typeof(JsonConverter))]
         |public readonly struct RpDateTime
         |{
         |    internal readonly $csDateTime Underlying;
         |
         |    public RpDateTime(int year, int month, int day, int hour, int minute, int second, int millisecond, DateTimeKind kind)
         |    {
         |        Underlying = new $csDateTime(year, month, day, hour, minute, second, millisecond, kind);
         |    }
         |
         |    public RpDateTime(long ticks, $csDateTimeKind kind)
         |    {
         |        Underlying = $baboonTimeFormats.TruncateToMilliseconds(ticks, kind);
         |    }
         |
         |    public RpDateTime(DateTime dateTime)
         |    {
         |        Underlying = $baboonTimeFormats.TruncateToMilliseconds(dateTime);
         |    }
         |
         |    public override int GetHashCode()
         |    {
         |        return Underlying.GetHashCode();
         |    }
         |
         |    public override bool Equals(object? obj)
         |    {
         |        if (obj is RpDateTime other)
         |        {
         |            return other.Underlying.ToUniversalTime() == Underlying.ToUniversalTime();
         |        }
         |
         |        return false;
         |    }
         |
         |    public override string ToString() => $baboonTimeFormats.ToString(this);
         |
         |    public long Ticks => Underlying.Ticks;
         |    public DateTimeKind Kind => Underlying.Kind;
         |    public RpDateTime ToUniversalTime() => new RpDateTime(Underlying.ToUniversalTime());
         |    public RpDateTime ToLocalTime() => new RpDateTime(Underlying.ToLocalTime());
         |    public RpDateTime LocalDate => new RpDateTime(Underlying.ToLocalTime().Date);
         |    public RpDateTime Date => new RpDateTime(Underlying.Date);
         |    public TimeSpan GetUtcOffset() => $csTimeZoneInfo.Local.GetUtcOffset(Underlying);
         |    public TimeSpan Subtract(RpDateTime right) => Underlying.ToUniversalTime().Subtract(right.Underlying);
         |    public RpDateTime Subtract(TimeSpan span) => new RpDateTime(Underlying.Subtract(span));
         |    public RpDateTime Add(TimeSpan value) => new RpDateTime(Underlying.Add(value));
         |    public RpDateTime AddTicks(long value) => new RpDateTime(Underlying.AddTicks(value));
         |    public RpDateTime AddMilliseconds(double value) => new RpDateTime(Underlying.AddMilliseconds(value));
         |    public RpDateTime AddSeconds(double value) => new RpDateTime(Underlying.AddSeconds(value));
         |    public RpDateTime AddMinutes(double value) => new RpDateTime(Underlying.AddMinutes(value));
         |    public RpDateTime AddHours(double value) => new RpDateTime(Underlying.AddHours(value));
         |    public RpDateTime AddDays(double value) => new RpDateTime(Underlying.AddDays(value));
         |    public RpDateTime AddMonths(int value) => new RpDateTime(Underlying.AddMonths(value));
         |    public RpDateTime AddYears(int value) => new RpDateTime(Underlying.AddYears(value));
         |
         |    public static RpDateTime Now => new RpDateTime($csDateTime.Now);
         |    public static RpDateTime UtcNow => new RpDateTime($csDateTime.UtcNow);
         |    public static RpDateTime Epoch => new RpDateTime(1970, 1, 1, 0, 0, 0, 0, $csDateTimeKind.Utc);
         |    public static RpDateTime MinValue => new RpDateTime($csDateTime.MinValue);
         |    public static RpDateTime MaxValue => new RpDateTime($csDateTime.MaxValue);
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
         |        return left.Underlying.ToUniversalTime() - right.Underlying.ToUniversalTime();
         |    }
         |
         |    public static bool operator >(RpDateTime left, RpDateTime right)
         |    {
         |        return left.Underlying.ToUniversalTime() > right.Underlying.ToUniversalTime();
         |    }
         |
         |    public static bool operator <(RpDateTime left, RpDateTime right)
         |    {
         |        return left.Underlying.ToUniversalTime() < right.Underlying.ToUniversalTime();
         |    }
         |
         |    public static bool operator <=(RpDateTime left, RpDateTime right)
         |    {
         |        return left.Underlying.ToUniversalTime() <= right.Underlying.ToUniversalTime();
         |    }
         |
         |    public static bool operator >=(RpDateTime left, RpDateTime right)
         |    {
         |        return left.Underlying.ToUniversalTime() >= right.Underlying.ToUniversalTime();
         |    }
         |
         |    public static implicit operator RpDateTime($csDateTime dt) => new(dt);
         |    public static implicit operator $csDateTime(RpDateTime dt) => dt.Underlying;
         |
         |    public static RpDateTime Parse($csString dt) => $baboonTimeFormats.FromString(dt);
         |
         |    private class JsonConverter : $nsJsonConverter<RpDateTime>
         |    {
         |        public override void WriteJson($nsJsonWriter writer, RpDateTime value, $nsJsonSerializer serializer)
         |        {
         |            writer.WriteValue($baboonTimeFormats.ToString(value));
         |        }
         |
         |        public override RpDateTime ReadJson($nsJsonReader reader, $csTpe objectType, RpDateTime existingValue, bool hasExistingValue, $nsJsonSerializer serializer)
         |        {
         |            return $baboonTimeFormats.FromString((string)reader.Value!);
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
         |        return dt.Underlying.ToString(dt.Underlying.Kind == $csDateTimeKind.Utc ? TsuDefault : TszDefault, $csInvariantCulture.InvariantCulture);
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

    val sharedRuntimeSource = Seq(baseCodecsSource, baseToolsSource, conversionKeySource, conversionsSource).join("\n\n")
    val sharedRuntime = tools.inNs(CSBaboonTranslator.baboonRtPkg.parts.toSeq, sharedRuntimeSource)
    val sharedOutput = CSDefnTranslator.Output(s"BaboonRuntimeShared.cs", sharedRuntime, CSBaboonTranslator.baboonRtPkg, isTest = false)

    val timeSource = Seq(rpDateTimeSource, timeFormatsSource).join("\n\n")
    val time = tools.inNs(CSBaboonTranslator.baboonTimePkg.parts.toSeq, timeSource)
    val timeOutput = CSDefnTranslator.Output(s"BaboonTime.cs", time, CSBaboonTranslator.baboonTimePkg, isTest = false)

    Right(List(
      sharedOutput,
      timeOutput
    ))
  }

  private def sharedTestRuntime: Out[List[CSDefnTranslator.Output]] = {
    val sharedTestRuntime =
      q"""
         |using System;
         |using System.Collections.Immutable;
         |using System.Reflection;
         |using AutoFixture.Kernel;
         |using AutoFixture;
         |using Baboon.Time;
         |
         |internal class TruncatedRandomDateTimeSequenceGenerator : ISpecimenBuilder
         |{
         |    private readonly ISpecimenBuilder innerRandomDateTimeSequenceGenerator;
         |
         |    internal TruncatedRandomDateTimeSequenceGenerator()
         |    {
         |        this.innerRandomDateTimeSequenceGenerator = new RandomDateTimeSequenceGenerator();
         |    }
         |
         |    public object Create(object request, ISpecimenContext context)
         |    {
         |        var result = this.innerRandomDateTimeSequenceGenerator.Create(request, context);
         |        if (result is NoSpecimen) {
         |            return result;
         |        }
         |        return BaboonDateTimeFormats.TruncateToMilliseconds((DateTime)result);
         |    }
         |}
         |
         |public class EnumDictionaryBuilder : ISpecimenBuilder
         |{
         |    public object? Create(object request, ISpecimenContext context)
         |    {
         |        var type = ExtractType(request);
         |        if (
         |          type == null ||
         |          !type.IsGenericType ||
         |          type.GetGenericTypeDefinition() != typeof(ImmutableDictionary<,>)
         |        )
         |        {
         |            return new NoSpecimen();
         |        }
         |
         |        var keyType = type.GetGenericArguments()[0];
         |        var valueType = type.GetGenericArguments()[1];
         |
         |        if (keyType.IsEnum)
         |        {
         |            var key = Enum.GetValues(keyType).GetValue(0);
         |            var value = context.Resolve(valueType);
         |
         |            Type immutableDictType = typeof(ImmutableDictionary<,>).MakeGenericType(keyType, valueType);
         |
         |            MethodInfo? createMethod = typeof(ImmutableDictionary)
         |                .GetMethod("Create", BindingFlags.Public | BindingFlags.Static, Type.EmptyTypes)
         |                ?.MakeGenericMethod(keyType, valueType);
         |
         |            var emptyImmutableDict = createMethod?.Invoke(null, null);
         |
         |            MethodInfo? addMethod = immutableDictType.GetMethod("Add", new[] { keyType, valueType });
         |
         |            return addMethod?.Invoke(emptyImmutableDict, new[] { key, value });
         |        }
         |
         |        return new NoSpecimen();
         |    }
         |
         |    private static Type? ExtractType(object request)
         |    {
         |        return request switch
         |        {
         |            Type type => type,
         |            PropertyInfo propertyInfo => propertyInfo.PropertyType,
         |            FieldInfo fieldInfo => fieldInfo.FieldType,
         |            ParameterInfo parameterInfo => parameterInfo.ParameterType,
         |            _ => null
         |        };
         |    }
         |}
         |""".stripMargin

    Right(
      List(
        CSDefnTranslator.Output(
          "BaboonTestRuntimeShared.cs",
          tools.inNs(CSBaboonTranslator.baboonTestRtPkg.parts.toSeq, sharedTestRuntime),
          CSBaboonTranslator.baboonTestRtPkg,
          isTest = true,
        )
      )
    )
  }

  private def generateConversions(
                                   domain: Domain,
                                   lineage: BaboonLineage,
                                   toCurrent: Set[EvolutionStep],
                                   defnOut: List[CSDefnTranslator.OutputExt]
                                 ): Out[List[CSDefnTranslator.Output]] = {
    val pkg = trans.toCsPkg(domain.id, domain.version, lineage.evolution, options)

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
           |public class BaboonConversions : $abstractBaboonConversions
           |{
           |    public BaboonConversions(RequiredConversions requiredConversions)
           |    {
           |        ${conversionRegs.join("\n").shift(8).trim}
           |    }
           |
           |    override public $csList<$csString> VersionsFrom()
           |    {
           |        return new $csList<$csString> { ${
          toCurrent
            .map(_.from.version)
            .map(v => s"""\"$v\"""")
            .mkString(", ")
        } };
           |    }
           |
           |    override public $csString VersionTo()
           |    {
           |        return "${domain.version.version}";
           |    }
           |}""".stripMargin

      val codecs =
        q"""public class BaboonCodecs : $abstractBaboonCodecs
           |{
           |    private BaboonCodecs()
           |    {
           |        ${defnOut.map(_.codecReg).join("\n").shift(8).trim}
           |    }
           |
           |    private static Lazy<BaboonCodecs> instance = new Lazy<BaboonCodecs>(() => new BaboonCodecs());
           |
           |    public static BaboonCodecs Instance { get { return instance.Value; } }
           |}""".stripMargin

      val basename = tools.basename(domain, lineage.evolution, options)

      val runtimeSource = Seq(converter, codecs).join("\n\n")
      val runtime = tools.inNs(pkg.parts.toSeq, runtimeSource)
      val runtimeOutput = CSDefnTranslator.Output(s"$basename/BaboonRuntime.cs", runtime, pkg, isTest = false)

      val convertersOutput = convs.map { conv =>
        CSDefnTranslator
          .Output(s"$basename/${conv.fname}", conv.conv, pkg, isTest = false)
      }

      List(
        runtimeOutput
      ) ++ convertersOutput
    }
  }

}

object CSBaboonTranslator {
  case class RenderedConversion(fname: String,
                                conv: TextTree[CSValue],
                                reg: Option[TextTree[CSValue]],
                                missing: Option[TextTree[CSValue]],
                               )

  val baboonRtPkg: CSPackageId = CSPackageId(NEList("Baboon", "Runtime", "Shared"))
  val baboonTestRtPkg: CSPackageId = CSPackageId(NEList("Baboon", "Test", "Runtime", "Shared"))
  val baboonTimePkg: CSPackageId = CSPackageId(NEList("Baboon", "Time"))

  val csSystemPkg: CSPackageId = CSPackageId(NEList("System"))
  val csGlobalizationPkg: CSPackageId = CSPackageId(NEList("System", "Globalization"))
  val csCollectionsGenericPkg: CSPackageId = CSPackageId(NEList("System", "Collections", "Generic"))
  val csCollectionsImmutablePkg: CSPackageId = CSPackageId(NEList("System", "Collections", "Immutable"))
  val csLinqPkg: CSPackageId = CSPackageId(NEList("System", "Linq"))
  val csIoPkg: CSPackageId = CSPackageId(NEList("System", "IO"))

  val nsPkg: CSPackageId = CSPackageId(NEList("Newtonsoft", "Json"))
  val nsLinqPkg: CSPackageId = CSPackageId(NEList("Newtonsoft", "Json", "Linq"))

  val abstractConversion: CSType =
    CSType(baboonRtPkg, "AbstractConversion", fq = false)
  val abstractBaboonConversions: CSType =
    CSType(baboonRtPkg, "AbstractBaboonConversions", fq = false)
  val iBaboonGenerated: CSType =
    CSType(baboonRtPkg, "IBaboonGenerated", fq = false)
  val iBaboonGeneratedLatest: CSType =
    CSType(baboonRtPkg, "IBaboonGeneratedLatest", fq = false)
  val BaboonTools: CSType =
    CSType(baboonRtPkg, "BaboonTools", fq = false)

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
  val baboonTypeCodecs: CSType =
    CSType(baboonRtPkg, "BaboonTypeCodecs", fq = false)
  val abstractBaboonCodecs: CSType =
    CSType(baboonRtPkg, "AbstractBaboonCodecs", fq = false)

  val baboonTimeFormats: CSType =
    CSType(baboonTimePkg, "BaboonDateTimeFormats", fq = false)

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

  val csTpe: CSType =
    CSType(csSystemPkg, "Type", fq = false)
  val csLazy: CSType =
    CSType(csSystemPkg, "Lazy", fq = false)
  val csList: CSType =
    CSType(csCollectionsGenericPkg, "List", fq = false)
  val csDict: CSType =
    CSType(csCollectionsGenericPkg, "Dictionary", fq = false)
  val csString: CSType =
    CSType(csSystemPkg, "String", fq = false)
  val csGuid: CSType =
    CSType(csSystemPkg, "Guid", fq = false)
  val csEnum: CSType =
    CSType(csSystemPkg, "Enum", fq = false)
  val csDateTime: CSType =
    CSType(csSystemPkg, "DateTime", fq = false)
  val rpDateTime: CSType =
    CSType(baboonTimePkg, "RpDateTime", fq = false)
  val csArgumentException: CSType =
    CSType(csSystemPkg, "ArgumentException", fq = false)
  val csEnumerable: CSType =
    CSType(csLinqPkg, "Enumerable", fq = false)

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

  val csNotImplementedException: CSType =
    CSType(
      CSPackageId("System.Runtime.Serialization"),
      "NotImplementedException",
      fq = false
    )
}
