open botModule

initializeBot()

// Each "post" is a map object containing the name of the author, the post's body text, and its title. (This can be customized using the filterList variable.)
let filterList = ["title"; "author"; "selftext"; "name"; "created_utc"]
// Get the latest 100 posts from the subreddit r/programmerhumor and return only the information indicated in the filterList.
let post_list = subredditGetPostsAndTimeFiltered "r/programmerhumor" "new" true filterList 1

// Give each post a number to keep track.
let mutable counter = 0

// Print all of the received posts, but only their post titles and the time since they were posted.
// Time is in D.HH.MM.SS format.
for i in post_list do
    counter <- counter + 1
    printfn $"""{counter}: {i["title"]} - {i["created_utc"] + "\n"}"""