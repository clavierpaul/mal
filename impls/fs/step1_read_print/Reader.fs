module MAL.Reader

open FParsec
open Types

let str s = pstring s

let malValue, malValueRef = createParserForwardedToRef<MalType, unit>()

let malNumber: Parser<MalType, unit> = pint32 |>> MalNumber

let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
            (spaces >>. sepEndBy pElement spaces |>> f)

let malList = listBetweenStrings "(" ")" malValue MalList

let malSymbol: Parser<MalType, unit> = many1Chars (asciiLetter <|> anyOf "+*-/") |>> MalSymbol

do malValueRef := choice [ malList; malNumber; malSymbol ]

let malParser = spaces >>. malValue .>> spaces .>> eof

let read_str str =
    match run malParser str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith $"Error parsing input: {errorMsg}"