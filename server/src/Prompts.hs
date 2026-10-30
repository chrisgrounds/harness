{-# LANGUAGE OverloadedStrings #-}

module Prompts
  ( designSystemPrompt,
    discoveryInstruction,
    typesInstruction,
    functionsInstruction,
    propertiesInstruction,
    revisionNote,
    brokenSignaturesNote,
    fixUpInstruction,
    reviewInstruction,
    callToolNudge,
    implementSystemPrompt,
    implementInstruction,
  )
where

import Core
import Data.Text (Text)
import qualified Data.Text as T
import Project (Component, describeComponent)

designSystemPrompt :: Text
designSystemPrompt =
  T.unlines
    [ "You are holey, a type-driven Haskell assistant working in the user's codebase (the current directory).",
      "You turn a requirement into changes to the codebase by first discovering and planning, then designing types, function signatures and QuickCheck properties.",
      "The changes can create new modules and change existing ones: add types and functions, change existing types, and change or rewrite existing functions.",
      "Integrate the work: if the requirement needs existing code to use the new code (a key binding, a call site, a new case), change those existing functions too.",
      "You never implement functions. After design, each function is implemented by a separate agent that sees only its module, the design, its signature and documentation, and the properties. So:",
      "- Make the types as precise as possible: newtypes for distinct values, sum types for alternatives, no stringly-typed data. An implementer must be able to rely on the types alone.",
      "- Prefer small pure functions; keep IO at the edges and only where the requirement needs it.",
      "- Write each function's documentation precisely enough to implement from it and the types alone. It is agent context only and is never written into the code.",
      "  Describe what the function does; for a changed existing function, say what to change in its separate change note. Existing source comments are preserved.",
      "- Express every invariant the types cannot capture as a property.",
      "Existing functions that stop type checking because of your changes are found and repaired automatically, keeping their signatures.",
      "Use ReadFile, ListFiles and RipGrep to understand the existing code before designing, and reuse its types where they fit.",
      "Batch independent ReadFile, ListFiles and RipGrep calls in one reply to explore in parallel.",
      "Each step tells you which tool records its result. Call it, then finish your turn with a one-line summary.",
      "If a step shows that an earlier step's result is wrong, call the earlier step's tool again with the correction before finishing the step;",
      "never carry a mistake you have found into implementation.",
      "SetTypes, SetFunctions and SetProperties record changes on top of what is already recorded: each module, function or property you list",
      "replaces the earlier one of the same name, anything you do not list stays as it is, and remove drops earlier ones."
    ]

discoveryInstruction :: Requirement -> [Component] -> Text
discoveryInstruction req components =
  T.unlines
    [ "Requirement:",
      requirementText req,
      "",
      "Components in this project:",
      T.unlines (map describeComponent components),
      "Step 1: discover and plan. Use ReadFile, ListFiles and RipGrep to inspect the existing code before choosing a design.",
      "Trace the relevant behaviour through its entry points and callers, including other components that must use the change.",
      "Identify reusable types and functions, the modules and functions that need to change, and how success will be checked.",
      "Call SetPlan with the required behaviour, findings with concrete code references, proposed changes and integration points,",
      "and any assumptions or unresolved questions. Keep the plan proportionate: a small task needs only a brief plan.",
      "On a redesign, revisit the findings and plan in light of the failure. Do not create types, signatures or implementations yet.",
      "Planning proceeds automatically into type design; no user approval is needed."
    ]

typesInstruction :: Requirement -> [Component] -> Text
typesInstruction req components =
  T.unlines
    [ "Requirement:",
      requirementText req,
      "",
      "Components in this project:",
      T.unlines (map describeComponent components),
      "Step 2: types. Use the discovery findings and plan, exploring further if needed, then call SetTypes with every module the requirement needs to create or change,",
      "including modules where you will only add or change functions. For each module give:",
      "- its component, named exactly as listed;",
      "- its module name: an existing module to change it, or a new name to create it;",
      "- any LANGUAGE extensions and import lines to add, using only packages the component already depends on (the build cannot add dependencies);",
      "- type declarations and instances only, without functions. In an existing module, give only new types and the complete new",
      "  declaration of each type you change; a declaration replaces the existing one of the same name. Leave out unchanged types.",
      "The first module is the main one: the property suite is named after it and built against its component. Once recorded, the main module stays first."
    ]

functionsInstruction :: Model -> Text
functionsInstruction run =
  T.unlines $
    [ "Step 3: functions. Call SetFunctions with every function to add or change, each with its module (from step 2),",
      "its name, its signature (the type only), documentation stating exactly what it does, and, for an existing function, a change note.",
      "The documentation is agent context only and is never written into the code; existing source comments are preserved.",
      "Describe the function as it will be. The separate change note tells its implementer what to change and is never written into the code.",
      "Naming an existing function in its module replaces it: its implementer sees the previous definition.",
      "Include existing functions whose behaviour must change even if their signature does not. Leave out functions that stay as they are.",
      "Functions may use each other. Do not implement them."
    ]
      ++ case implementers run of
        offered@(_ : _ : _) ->
          [ "Choose each function's implementer with its model field: " <> T.intercalate ", " (map modelName offered) <> ", from cheapest to most capable.",
            "Pick the cheapest that will get it right: " <> modelName (head offered) <> " for short definitions that follow from the types,",
            "and the most capable only for intricate logic. Without a choice, " <> modelName (defaultImplementer run) <> " implements it.",
            "A function that fails is reimplemented by the next more capable model."
          ]
        _ -> []

propertiesInstruction :: Text
propertiesInstruction =
  T.unlines
    [ "Step 4: properties. Call SetProperties with QuickCheck properties for the invariants the types cannot express.",
      "The property module is built against the first module's component. It already imports Test.QuickCheck and every module from step 2",
      "that component can use: its own modules and those of libraries it depends on (never an executable's Main). It cannot use the others.",
      "It can only use names those modules export: everything in new modules, and in existing modules what they already export",
      "plus the types and functions from steps 2 and 3. To use another existing type, repeat its declaration unchanged in step 2.",
      "Put any other imports in imports, and Arbitrary instances or generators for the new types in helpers.",
      "Each property needs a name starting with prop_ and its full source: signature and definition.",
      "Test the requirement's behaviour, not definitions restated: a property that only repeats a constant or a function body checks nothing.",
      "Properties may only use the types and the functions from step 3, and existing code."
    ]

revisionNote :: Int -> Text -> Text
revisionNote iterationNumber feedback =
  T.unlines
    [ "Iteration " <> T.pack (show iterationNumber) <> ": the previous design failed verification:",
      feedback,
      "Revise the design to fix this. The previous design is still recorded, so give each step only what changes:",
      "list only new or changed modules, functions and properties, and remove those to drop. If a step needs no change, call its tool with nothing listed.",
      "Functions whose signature, documentation and change note stay the same keep their implementations.",
      ""
    ]

brokenSignaturesNote :: [FunctionRef] -> Text
brokenSignaturesNote refs =
  T.unlines
    [ "The signatures of these existing functions no longer type check with the new design: "
        <> T.intercalate ", " [functionNameText (refName r) <> " (" <> T.pack (refPath r) <> ")" | r <- refs]
        <> ".",
      "Add them to SetFunctions with new signatures, or keep the design compatible with them."
    ]

fixUpInstruction :: ToolName -> Text -> Text
fixUpInstruction tool errors =
  T.unlines
    [ "The skeleton does not type check, because of what this step wrote:",
      errors,
      "Fix it by calling " <> toolNameText tool <> " again with the corrected " <> what <> ": each one you list replaces the earlier one,"
        <> " and remove drops any that should go. The rest of the design stays as it is."
    ]
  where
    what = case tool of
      SetTypes -> "modules and types"
      SetFunctions -> "functions"
      _ -> "properties"

reviewInstruction :: Requirement -> [ModulePlan] -> [ComponentName] -> [(ModuleName, [FunctionSpec])] -> Text
reviewInstruction req plans untouched written =
  T.unlines $
    [ "Step 5: review. The run's code type checks and its properties pass, but the properties cannot check everything the requirement asks for.",
      "The requirement was:",
      requirementText req,
      "",
      "The run changed only these modules:"
    ]
      ++ ["- " <> moduleNameText (planModule p) <> " in " <> componentNameText (planComponent p) <> (if planIsNew p then " (new)" else "") | p <- plans]
      ++ ["It changed nothing in: " <> T.intercalate ", " (map componentNameText untouched) <> "." | not (null untouched)]
      ++ [ "",
           "These are the functions the run wrote:",
           ""
         ]
      ++ concat
        [ ["```haskell", "-- " <> moduleNameText m]
            ++ concat [[functionNameText (fnName f) <> " :: " <> fnSignature f | not (T.null (fnSignature f))] ++ maybe [] T.lines (fnBody f) ++ [""] | f <- fns]
            ++ ["```", ""]
        | (m, fns) <- written
        ]
      ++ [ "First, check them against the requirement, phrase by phrase: does the code do everything it asks, where and when it asks?",
           "Pay attention to what properties cannot test, such as when something happens, what reaches the user, and which code path is taken.",
           "Then look for duplication: logic repeated across these functions, or code that already exists elsewhere in the codebase (use ReadFile and RipGrep).",
           "If anything falls short or is duplicated and new definitions of these functions can fix it, call Refactor with them, and any helpers they should share.",
           "Each definition must define only its function. The refactor is kept only if the code still builds and the properties still pass.",
           "If the requirement is not met and fixing it needs more than that, such as changes to other modules or components, call RequirementNotMet",
           "saying what is missing and where; the run then goes back to design.",
           "If the code meets the requirement and nothing is duplicated, reply with a one-line summary of your review without calling a tool."
         ]

callToolNudge :: ToolName -> Text
callToolNudge name = "You have not called " <> toolNameText name <> " successfully yet. Call it now to complete this step."

implementSystemPrompt :: Text
implementSystemPrompt =
  T.unlines
    [ "You implement exactly one Haskell function in a module of an existing codebase.",
      "You are given the design (every new and changed type and the signature and documentation of every designed function),",
      "the module the function is in (other functions being implemented may be undefined), the function to implement,",
      "its previous definition if it existed, and the QuickCheck properties it must satisfy.",
      "Do not change any type or signature. You may use the module's other functions and anything it imports.",
      "Call ImplementFunction with the complete definition (every equation, no type signature) and any import lines it needs",
      "beyond the module's imports, using only packages the module already uses. Then finish your turn."
    ]

implementInstruction :: Text -> Text -> Text -> FunctionSpec -> [PropertySpec] -> Maybe Text -> Text
implementInstruction design existing moduleSource fn props feedback =
  T.unlines $
    [ "The design:",
      "```haskell",
      design,
      "```",
      ""
    ]
      ++ ( if T.null (T.strip existing)
             then []
             else ["Existing types its signature uses, as the project declares them:", "```haskell", T.stripEnd existing, "```", ""]
         )
      ++ [ "The module, " <> T.pack (refPath (fnRef fn)) <> ":",
      "```haskell",
      moduleSource,
      "```",
      "",
      if instanceRepair
        then "Rewrite " <> functionNameText (fnName fn) <> " so it type checks again: give its complete declaration, from the instance head to its methods, as the definition."
        else "Implement " <> functionNameText (fnName fn) <> case fnKind fn of
          Designed -> " :: " <> fnSignature fn
          Repair -> " again, keeping its signature in the module",
      case fnKind fn of
        Designed -> fnDoc fn <> if T.null (fnChange fn) then "" else "\nWhat to change: " <> fnChange fn
        Repair
          | instanceRepair -> "This existing instance stopped type checking because the design changed. Keep what it did, adapted to the new design."
          | otherwise -> "This existing function stopped type checking because the design changed. Keep what it did, adapted to the new design.",
      ""
    ]
      ++ maybe [] (\previous -> ["Its previous definition:", "```haskell", T.stripEnd previous, "```", ""]) (fnPrevious fn)
      ++ ( if null props
             then []
             else ["Properties it must satisfy:", "```haskell", T.unlines (map propSource props), "```", ""]
         )
      ++ case feedback of
        Just text -> ["A previous implementation failed:", text, "Fix it."]
        Nothing -> []
  where
    instanceRepair = isInstanceName (fnName fn)
