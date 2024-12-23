package io.septimalmind.baboon

import java.nio.file.Path

final case class CSOptions(generic: GenericOptions,
                           omitMostRecentVersionSuffixFromPaths: Boolean,
                           omitMostRecentVersionSuffixFromNamespaces: Boolean,
                           csUseCompactAdtForm: Boolean,
                           csWrappedAdtBranchCodecs: Boolean,
                           csWriteEvolutionDict: Boolean,
                           csDisregardImplicitUsings: Boolean,
)

final case class GenericOptions(obsoleteErrors: Boolean,
                                runtime: RuntimeGenOpt,
                                generateConversions: Boolean,
                                metaWriteEvolutionJsonTo: Option[Path],
                                codecTestIterations: Int,
)

final case class CompilerOptions(debug: Boolean, csOptions: CSOptions)
