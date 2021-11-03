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

let malList = listBetweenStrings "(" ")" malValue MalList

let malSymbol: Parser<MalType, unit> = many1Chars (asciiLetter <|> digit <|> anyOf "+*-/>") |>> MalSymbol

do malValueRef := choice [ malList; malNumber; malSymbol ]

let malParser = ws >>. malValue .>> ws .>> eof

let read_str str =
    match run malParser str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith $"Error parsing input: {errorMsg}"