module botModule

open System
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Net.Http
open System.Net.Http.Headers
open botInfo // Used solely to hide sensitive information, like the bot's username, password, etc.

(*
  How data is structered when recieved by requests:

  The basic structure goes like this: Whenever a successful request has been made and the JSON data has been recieved,
  it'll be firstly be parsed into a sequence of JSONnodes and then all relevant nodes will be filtered into a list of maps (pairs of, for example, "title"; "{title}").
  The list is a collection of Reddit posts where the ordered map contains the information of the indexed posts.
  For example, list[0]["title"] contains the title of the first post in the list, where list[0]["author"] would be the user who created that post.
*)

let client = new HttpClient()

client.DefaultRequestHeaders.TryAddWithoutValidation("User-Agent", userInfo.["User-Agent"])

let constructHttpRequest (method : string) (uri : string) =
  new HttpRequestMessage(new HttpMethod(method), uri)

let constructEncodedContent (content : Map<string, string>) =
  new FormUrlEncodedContent(content)

let setHttpRequestMessageContent (httpRequest : HttpRequestMessage) (httpContent : FormUrlEncodedContent) =
  httpRequest.Content <- httpContent
  httpRequest

let stringToBase64 (s : string) =
  let Encoder = UTF8Encoding()
  let bytesFromString = Encoder.GetBytes(s)
  Convert.ToBase64String(bytesFromString)

let constructAuthenticationHeaderValue (scheme : string) (credentials : string) =
  new AuthenticationHeaderValue(scheme, credentials |> stringToBase64)

let getRedditAccessToken (postData : Map<string, string>) (ID : string) (secret : string) =
  let request = (constructHttpRequest "POST" "https://www.reddit.com/api/v1/access_token", constructEncodedContent postData) ||> setHttpRequestMessageContent
  request.Headers.Authorization <- ("Basic", $"{ID}:{secret}") ||> constructAuthenticationHeaderValue
  task {
    let! response = client.SendAsync(request, HttpCompletionOption.ResponseContentRead)
    return response.Content
  }

let getStringFromContent (content : HttpContent) =
  task {
    return! content.ReadAsStringAsync()
  }

let constructJsonNode (s : string) =
  let mutable OPTION = new JsonDocumentOptions()
  OPTION.MaxDepth <- 64
  JsonNode.Parse(s, documentOptions=OPTION)

let createTextFile (path : string) (s : string) =
  task {
    return! File.WriteAllTextAsync(path, s)
  }
(*
let truncateTextFile (path : string) (s : string) =
  let ENCODER = new UnicodeEncoding()
  let STREAM = new FileStream(path, FileMode.Truncate, FileAccess.ReadWrite)
  task {
    return! STREAM.WriteAsync(ENCODER.GetBytes(s))
  }
*)
let readTextFile (path : string) =
  task {
    return! File.ReadAllTextAsync(path)
  }

let setAccessToken (s : string) =
  client.DefaultRequestHeaders.Authorization <- new AuthenticationHeaderValue("Bearer", s)

let checkResponse (message : HttpResponseMessage) =
  match message with
  | message when message.IsSuccessStatusCode -> true
  | _ -> false

let checkRateLimit (message : HttpResponseMessage) =
  let HEADERS = message.Headers.GetValues("x-ratelimit-remaining") |> Seq.exactlyOne
  let VALUE = int (float HEADERS)
  match VALUE with
  | VALUE when VALUE >= 1 -> true
  | _ -> false

let saveNewAccessToken (responseCheck : bool) =
  let accessTokenFormat = Map [("grant_type", "password"); ("username", userInfo["username"]); ("password", userInfo["password"])]
  match responseCheck with
  | true -> true
  | false ->
    let GET = getRedditAccessToken accessTokenFormat userInfo.["ID"] userInfo.["secret"]
    let READ = GET.Result |> getStringFromContent
    let CONVERT = READ.Result |> constructJsonNode
    let TOKEN = CONVERT.Item("access_token").ToString()
    let WRITE = ("access_token.txt", TOKEN) ||> createTextFile
    TOKEN |> setAccessToken
    WRITE.Wait()
    printfn "New access token acquired, written to file, and set on header!"
    false

let responseChecker = checkResponse >> saveNewAccessToken

let postAsyncRequest (uri : string) (content : Map<string, string>) =
  task {
    try
      return! client.PostAsync(uri, content |> constructEncodedContent)
    with
      | :? HttpRequestException as e ->
        printfn $"Request failed:\n{e.Message}"
        return new HttpResponseMessage(e.StatusCode.GetValueOrDefault())
  }

let getAsyncRequest (uri : string) (content : Map<string, string>) =
  task {
    try
      return! client.SendAsync((constructHttpRequest "GET" uri, constructEncodedContent content) ||> setHttpRequestMessageContent)
    with
      | :? HttpRequestException as e ->
        printfn $"Request failed:\n{e.Message}"
        return new HttpResponseMessage(e.StatusCode.GetValueOrDefault())
  }

let AttemptLoop (uri : string) (method : string) (content : Map<string, string>) =
  let ARGCHECK =
    match method with
    | "POST"-> ()
    | "GET" -> ()
    | _ -> failwith "Method is not a POST or GET!!"
  let rec loop n response =
    match n with
    | 0 -> response
    | _ ->
      let REQUEST =
        match method with
        | "POST" -> (uri, content) ||> postAsyncRequest
        | "GET" -> (uri, content) ||> getAsyncRequest
        | _ -> failwith "Either the wrong METHOD was picked or an unexpected error happened!"
      let TOKENCHECK = REQUEST.Result |> responseChecker
      let RATECHECK =
        match TOKENCHECK with
        | true -> REQUEST.Result |> checkRateLimit
        | false -> false
      match TOKENCHECK with
      | true when RATECHECK = true -> loop (n - 1) REQUEST.Result
      | false -> loop n REQUEST.Result
      | _ -> failwith $"Something unexpected happened! Perhaps an x-ratelimit?:\n{REQUEST.Result}"
  loop 1 (new HttpResponseMessage())

let readHttpContent (response : HttpResponseMessage) =
  let READ = response.Content |> getStringFromContent
  READ.Result

let responseToJsonNode (response : HttpResponseMessage) =
  response |> readHttpContent |> constructJsonNode

let readContentKey (response : HttpResponseMessage) (key : string) =
  let NODE = response |> responseToJsonNode
  NODE.Item(key).ToString()

let redditParser (response : HttpResponseMessage) =
  let STARTNODE = response |> responseToJsonNode
  let ARRAY = STARTNODE["data"].Item("children").AsArray()
  let rec loop n sequ =
    match n with
    | n when n = ARRAY.Count -> sequ
    | _ -> loop (n + 1) (Seq.append sequ (Seq.singleton ARRAY[n]))
  loop 0 Seq.empty

let getNodeInfo (sequ : seq<JsonNode>) (nodeIndex : int) (infoSelection : string) =
  let NODE = Seq.item nodeIndex sequ
  NODE["data"].Item(infoSelection).ToString()

let filterNodeData (sequ : seq<JsonNode>) (nodeIndex : int) (nodesToBeIncluded : string list) =
  let rec loop = function
    | [] -> Map.empty
    | x::xs -> (loop xs) |> Map.add x (getNodeInfo sequ nodeIndex x)
  loop nodesToBeIncluded

let putFilteredDataIntoList (sequ : seq<JsonNode>) (nodesToBeIncluded : string list) =
  let LENGTH = Seq.length sequ
  let rec loop n l =
    match n with
    | n when n = LENGTH -> l
    | _ -> loop (n + 1) (l @ [filterNodeData sequ n nodesToBeIncluded])
  loop 0 []

let sendPrivateMessage (subject : string) (text : string) (recipient : string) =
  AttemptLoop "https://oauth.reddit.com/api/compose" "POST" (Map [("subject", $"{subject}"); ("text", $"{text}"); ("to", $"{recipient}")])

let postsFromSubreddit (subreddit : string) (sortMethod : string) (nsfw : bool) (limit : int) =
  let ARGCHECK =
    match sortMethod with
    | "new" -> ()
    | "hot" -> ()
    | _ -> failwith "Limit value is invalid!"
  let NSFWCHECK =
    match nsfw with
    | true -> 1
    | false -> 0
  match limit with
  | limit when limit > 100 || limit <= 0 -> failwith $"""Limit value {limit} is invalid! It has to be either "hot" or "new"."""
  | _ -> AttemptLoop ("https://oauth.reddit.com/r/" + $"{subreddit}/{sortMethod}?limit={limit}&include_over_18={NSFWCHECK}") "GET" (Map [("", "")])

let containsString (contains : string) (s : string) =
  s.Contains(contains)

let keyContainsValue (key : string) (keyword : string) (map : Map<string, string>) =
  map[key] |> containsString keyword

let mapContainsValue (keyword : string) (map : Map<string, string>) =
  Map.values map |> Seq.findIndex (fun i -> i |> containsString keyword)

let requestParseAndFilter subreddit sortMethod nsfw limit nodesToBeIncluded =
  (postsFromSubreddit subreddit sortMethod nsfw limit |> redditParser, nodesToBeIncluded) ||> putFilteredDataIntoList

let initializeBot () =
  let READ = readTextFile "access_token.txt"
  setAccessToken READ.Result
  printfn $"The access token has been successfully set to {READ.Result}"