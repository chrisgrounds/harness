{-# LANGUAGE OverloadedStrings #-}

module Prompts
  ( designSystemPrompt,
    typesInstruction,
    functionsInstruction,
    propertiesInstruction,
    revisionNote,
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
      "You turn a requirement into a new Haskell module in three design steps: types, then function signatures, then QuickCheck properties.",
      "You never implement functions. After design, each function is implemented by a separate, cheaper agent that sees only the types, the signatures and documentation of the module's functions, and the properties. So:",
      "- Make the types as precise as possible: newtypes for distinct values, sum types for alternatives, no stringly-typed data. An implementer must be able to rely on the types alone.",
      "- Prefer small pure functions; keep IO at the edges and only where the requirement needs it.",
      "- Write each function's documentation precisely enough to implement from it and the types alone.",
      "- Express every invariant the types cannot capture as a property.",
      "Use ReadFile, Discover and Search to understand the existing code before designing, and reuse its types where they fit.",
      "Each step tells you which tool records its result. Call it, then finish your turn with a one-line summary."
    ]

typesInstruction :: Requirement -> [Component] -> Text
typesInstruction req components =
  T.unlines
    [ "Requirement:",
      requirementText req,
      "",
      "Components in this project that can hold the new module:",
      T.unlines (map describeComponent components),
      "Step 1: types. Explore the code as needed, then call SetTypes with:",
      "- the component the new module belongs to, named exactly as listed: the library or executable whose code the requirement concerns;",
      "- a new module name that does not exist yet;",
      "- any LANGUAGE extensions;",
      "- import lines, using only packages the component already depends on (the build cannot add dependencies);",
      "- the type declarations and instances only, without functions."
    ]

functionsInstruction :: Text
functionsInstruction =
  T.unlines
    [ "Step 2: functions. Call SetFunctions with every function the module needs to meet the requirement:",
      "its name, its signature (the type only), and documentation stating exactly what it does.",
      "Functions may use each other. Do not implement them."
    ]

propertiesInstruction :: Text
propertiesInstruction =
  T.unlines
    [ "Step 3: properties. Call SetProperties with QuickCheck properties for the invariants the types cannot express.",
      "The property module already imports the new module and Test.QuickCheck. Put any other imports in imports,",
      "and Arbitrary instances or generators for the new types in helpers.",
      "Each property needs a name starting with prop_ and its full source: signature and definition.",
      "Properties may only use the types and the functions from step 2."
    ]

-- | Prepended to a design step's instruction when a previous attempt failed.
revisionNote :: Int -> Text -> Text
revisionNote iterationNumber feedback =
  T.unlines
    [ "Iteration " <> T.pack (show iterationNumber) <> ": the previous design failed verification:",
      feedback,
      "Revise the design to fix this. Call the step's tool again even if this part is unchanged.",
      ""
    ]

callToolNudge :: ToolName -> Text
callToolNudge name = "You have not called " <> toolNameText name <> " successfully yet. Call it now to complete this step."

implementSystemPrompt :: Text
implementSystemPrompt =
  T.unlines
    [ "You implement exactly one Haskell function in an existing module.",
      "You are given the module (types, imports, and every function's signature and documentation; other bodies may still be undefined),",
      "the function to implement, and the QuickCheck properties it must satisfy.",
      "Do not change any type or signature. You may use the module's other functions.",
      "Call ImplementFunction with the complete definition (every equation, no type signature) and any import lines it needs",
      "beyond the module's imports, using only packages the module already uses. Then finish your turn."
    ]

implementInstruction :: Text -> FunctionSpec -> [PropertySpec] -> Maybe Text -> Text
implementInstruction skeleton fn props feedback =
  T.unlines $
    [ "The module:",
      "```haskell",
      skeleton,
      "```",
      "",
      "Implement " <> functionNameText (fnName fn) <> " :: " <> fnSignature fn,
      fnDoc fn,
      ""
    ]
      ++ ( if null props
             then []
             else ["Properties it must satisfy:", "```haskell", T.unlines (map propSource props), "```", ""]
         )
      ++ case feedback of
        Just text -> ["A previous implementation failed:", text, "Fix it."]
        Nothing -> []
