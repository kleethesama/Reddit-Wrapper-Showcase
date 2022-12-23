module httpTasks

open System
open System.Text
open System.Net.Http
open System.Net.Http.Headers
open botInfo

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

let getStringFromContent (content : HttpContent) =
  task {
    return! content.ReadAsStringAsync()
  }

let checkResponse (message : HttpResponseMessage) =
  match message with
  | message when message.IsSuccessStatusCode -> true
  | _ -> false

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

let readHttpContent (response : HttpResponseMessage) =
  let READ = response.Content |> getStringFromContent
  READ.Result