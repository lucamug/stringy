module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import CognitiveComplexity
import NoAlways
import NoDuplicatePorts
import NoEmptyText
import NoExposingEverything
import NoFloatIds
import NoImportingEverything
import NoInconsistentAliases
import NoMissingSubscriptionsCall
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeConstructor
import NoMissingTypeExpose
import NoModuleOnExposedNames
import NoPrematureLetComputation
import NoPrimitiveTypeAlias
import NoRecursiveUpdate
import NoSimpleLetBody
import NoSinglePatternCase
import NoTypeAliasConstructorCall
import NoUnapprovedLicense
import NoUnoptimizedRecursion
import NoUnsafePorts
import NoUnsortedConstructors
import NoUnsortedRecordFields
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUnusedPorts
import NoUselessSubscriptions
import Review.Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ NoExposingEverything.rule
    , NoImportingEverything.rule [ "Element", "Element.WithContext" ]
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoMissingTypeExpose.rule
    , NoMissingTypeConstructor.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoUnusedPorts.rule
    , NoSimpleLetBody.rule
    , NoAlways.rule
    , NoUselessSubscriptions.rule
    , NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
    , NoPrematureLetComputation.rule

    -- , NoUnsortedConstructors.rule
    -- , NoUnsortedRecordFields.rule
    , NoRecursiveUpdate.rule
    , NoInconsistentAliases.config
        [ ( "Element.Font", "Font" )
        , ( "Element.Border", "Border" )
        , ( "Element.Background", "Background" )
        , ( "Element.Events", "Events" )
        , ( "Element.Input", "Input" )
        ]
        |> NoInconsistentAliases.noMissingAliases
        |> NoInconsistentAliases.rule
    , NoModuleOnExposedNames.rule
    , NoTypeAliasConstructorCall.rule
    , NoPrimitiveTypeAlias.rule
    , NoUnapprovedLicense.rule
        { allowed = [ "BSD-3-Clause", "MIT", "Apache-2.0", "ISC", "MPL-2.0" ]
        , forbidden = [ "GPL-3.0-only", "GPL-3.0-or-later" ]
        }
    ]
        |> List.map
            (\rule ->
                rule
                    |> Review.Rule.ignoreErrorsForDirectories
                        []
                    |> Review.Rule.ignoreErrorsForFiles
                        []
            )
