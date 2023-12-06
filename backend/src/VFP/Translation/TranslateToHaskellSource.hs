module VFP.Translation.TranslateToHaskellSource(translateTypedToHaskell, translateUntypedToHaskell, translateToHaskellCode) where
import VFP.UI.UIModel
import Data.List
import Data.Char

brace :: String -> String
brace str = "(" ++ str ++ ")"

quote :: String -> String
quote str = "\"" ++ str ++ "\""

space :: String
space = " "

printHaskellType :: Type -> String
printHaskellType (Primitive n) = "Primitive " ++ space ++ quote n
printHaskellType (Generic num) = "Generic " ++ show num
printHaskellType (List inner) = "List " ++ brace (printHaskellType inner)
printHaskellType (Function from to) = "Function " ++ brace (printHaskellType from) ++ brace (printHaskellType to)

printHaskellMaybeType :: Maybe Type -> String
printHaskellMaybeType Nothing = "Nothing"
printHaskellMaybeType (Just t) = "Just (" ++ printHaskellType t ++ ")"

printHaskellUntypedArguments :: UntypedArguments -> String
printHaskellUntypedArguments UnknownArgs = "UnknownArgs"
printHaskellUntypedArguments (ToFill inner) = "ToFill " ++ brace (printHaskellType inner)
printHaskellUntypedArguments (ArgumentList args) = "ArgumentList [" ++ intercalate "," (map translateUntypedToHaskell args) ++ "]"

translateUntypedToHaskell :: UntypedValue -> String
translateUntypedToHaskell TypeHole =
    "TypeHole"
translateUntypedToHaskell (ValueDefinition typ name inner) =
    "ValueDefinition " ++ brace (printHaskellMaybeType typ) ++ space ++ quote name ++ space ++ brace (translateUntypedToHaskell inner)
translateUntypedToHaskell (Lambda typ body) =
    "Lambda " ++ brace (printHaskellMaybeType typ) ++ space ++ brace (translateUntypedToHaskell body)
translateUntypedToHaskell (Reference typ name args) =
    "Reference " ++ brace (printHaskellMaybeType typ) ++ space ++ quote name ++ space ++ brace (printHaskellUntypedArguments args)
translateUntypedToHaskell (IntegerLiteral str) =
    "IntegerLiteral " ++ brace (show (fmap quote str))
translateUntypedToHaskell (BooleanLiteral str) =
    "BooleanLiteral " ++ quote str
translateUntypedToHaskell (StringLiteral str) =
    "StringLiteral " ++ brace (show (fmap quote str))

translateTypedToHaskell :: TypedValue -> String
translateTypedToHaskell (TypedValueDefinition typ name inner) =
    "TypedValueDefinition " ++ brace (printHaskellType typ) ++ space ++ quote name ++ space ++ brace (translateTypedToHaskell inner)
translateTypedToHaskell (TypedTypeHole typ name) =
    "TypedTypeHole " ++ brace (brace $ printHaskellType typ) ++ space ++ quote name
translateTypedToHaskell (TypedLambda typ (ptyp, pname) body) =
    "TypedLambda " ++ brace (printHaskellType typ) ++ " (" ++ printHaskellType ptyp ++ "," ++ quote pname ++ ") " ++ brace (translateTypedToHaskell body)
translateTypedToHaskell (TypedReference typ name args) =
    "TypedReference " ++ brace (printHaskellType typ) ++ space ++ quote name ++ space ++ "[" ++ intercalate "," (map (brace . translateTypedToHaskell) args) ++ "]"
translateTypedToHaskell (TypedLiteral typ name) =
    "TypedLiteral " ++ brace (printHaskellType typ) ++ space ++ quote name

translateToHaskellCode :: TypedValue -> String
translateToHaskellCode (TypedValueDefinition typ name inner) = name ++ " :: " ++ (translateTypeToHaskellType typ) ++ "\n" ++ name ++ " = " ++ (translateToHaskellCode inner)
translateToHaskellCode (TypedTypeHole _ _) = error "No type holes allowed"
translateToHaskellCode (TypedLambda _ (_, pname) body) = brace $ "\\" ++ pname ++ " -> " ++ translateToHaskellCode body
translateToHaskellCode (TypedReference _ "if" [condition, thenBody, elseBody]) = "if " ++ translateToHaskellCode condition ++ "\n\tthen " ++ translateToHaskellCode thenBody ++ "\n\telse " ++ translateToHaskellCode elseBody
translateToHaskellCode (TypedReference _ name args) = name ++ " " ++ intercalate " " (map translateToHaskellCode args)
translateToHaskellCode (TypedLiteral typ name) = case typ of
    (Primitive _) -> name
    _ -> error $ "Unsuppported literal type " ++ show typ

translateTypeToHaskellType :: Type -> String
translateTypeToHaskellType (Primitive n) = n
translateTypeToHaskellType (Generic num) = [chr (ord 'a' - 1 + num)]
translateTypeToHaskellType (List inner) = "[" ++ translateTypeToHaskellType inner ++ "]"
translateTypeToHaskellType (Function from to) = translateTypeToHaskellType from ++ " -> " ++ translateTypeToHaskellType to