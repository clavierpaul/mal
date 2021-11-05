module MAL.Reader

open FParsec
open Types

let str s = pstring s

let ws: Parser<string, unit> = manyChars (anyOf " \n\r\t,")

let malValue, malValueRef = createParserForwardedToRef<MalType, unit>()

let malNumber: Parser<MalType, unit> = pint32 |>> MalNumber

let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
            (ws >>. sepEndBy pElement ws |>> f)
    <??> $"Opening \"{sOpen}\" unbalanced, missing closing \"{sClose}\""

let malList = listBetweenStrings "(" ")" malValue MalList

let malTrue: Parser<MalType, unit> = stringReturn "true" <| MalBool true
let malFalse: Parser<MalType, unit> = stringReturn "false" <| MalBool false
let malBool: Parser<MalType, unit> = malTrue <|> malFalse
let malNil: Parser<MalType, unit> = stringReturn "nil" <| MalNil

let stringLiteral =
    let escape =  anyOf "\"\\n"
                  |>> function
                      | 'n' -> "\n"
                      | c   -> string c // every other char is mapped to itself

    let escapedCharSnippet = str "\\" >>. escape
    let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

    between (str "\"") (str "\"")
            (stringsSepBy normalCharSnippet escapedCharSnippet)
    <??> "Unexpected EOF while reading string literal"

let malString: Parser<MalType, unit> = stringLiteral |>> MalString

let malSymbol: Parser<MalType, unit> =
    many1Chars (noneOf "{}()'`~^@\" \t\r\n" <|> fail "Unexpected EOF while reading program") |>> MalSymbol

do malValueRef := choice [ malList; malNumber; malBool; malNil; malString; malSymbol ]

let malParser = ws >>. malValue .>> ws .>> eof

let constructErrorMsg str = function
    | CompoundError (msg, _, _, _) -> str + msg
    | Message msg                  -> str + msg
    | _                            -> str
    
let read_str str =
    match run malParser str with
    | Success (result, _, _)   -> result
    // TODO: Actual error reporting
    | Failure (msg, error, _) ->
        let message = Array.fold constructErrorMsg "" <| ErrorMessageList.ToSortedArray error.Messages
        failwith message