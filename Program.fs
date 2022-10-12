open botModule

// Each "post" is a map object containing the name of the author, the post's body text, and its title. (This can be customized using the filterList variable.)

initializeBot
let filterList = ["title"; "author"; "selftext"]
let INIT = requestParseAndFilter "programmerhumor" "new" true 100 filterList

// print 3 posts from the "new" category.
for i = 0 to 2 do
    printfn $"{INIT[i]}"