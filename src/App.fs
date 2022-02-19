module App

open Elmish
open Elmish.React
open Feliz
open Fable.SimpleHttp
open Thoth.Json

let storyDecoder : Decoder<Story> =
    Decode.object (fun fields -> 
        {
            id = fields.Required.At [ "id" ] Decode.int
            title = fields.Required.At [ "title" ] Decode.string
            url = fields.Optional.At [ "url" ] Decode.string  
            score = fields.Required.At [ "score" ] Decode.int  
        }
    )

let loadStory (id : int) = async {
    let storiesEndpoint = sprintf "https://hacker-news.firebaseio.com/v0/item/%d.json" id
    let! (status, responseText) = Http.get storiesEndpoint
    match status with
    | 200 -> 
        match Decode.fromString storyDecoder responseText with
        | Ok story -> return LoadStoryItem (id, Ok story)
        | Error parseError -> return LoadStoryItem (id, Error parseError)
    | _ -> return LoadStoryItem (id, Error ("Http loading error" + string id))
}

let loadStories tab = 
    
    let categoryString = 
        match tab with
        | Tab.New -> "new"
        | Tab.Top -> "top"
        | Tab.Best -> "best"
        | Tab.Jobs -> "job"

    let storiesEndpoint = sprintf "https://hacker-news.firebaseio.com/v0/%sstories.json" categoryString
    
    async {
        let! (status, responseText) = Http.get storiesEndpoint
        match status with
        | 200 -> 
            let storyIds = Decode.fromString (Decode.list Decode.int) responseText
            match storyIds with
            | Ok ids -> 
                let idList = ids |> List.truncate 10
                return LoadStoryIds (Finished (Ok idList))

            | Error err ->
                return LoadStoryIds (Finished (Error err))
        | _ ->
            return LoadStoryIds (Finished (Error responseText))
    }

let init() =
    let newState = 
        { 
            Stories = NotStartedYet
            ActiveTab = Tab.New
        }

    newState, Cmd.ofMsg (LoadStoryIds Started)


let update (msg: Msg) (state: State) =
    match msg with
    | LoadStoryIds Started ->
        ({ state with Stories = InProgress }, Cmd.fromAsync (loadStories state.ActiveTab))

    | LoadStoryIds (Finished (Ok ids)) ->
        let map = Map.ofList [ for id in ids -> (id, InProgress) ]
        let commands = [ for id in ids -> Cmd.fromAsync (loadStory id) ]
        ({ state with Stories = Resolved (Ok map) }, Cmd.batch commands)

    | LoadStoryIds (Finished (Error err)) ->
        ({ state with Stories = Resolved (Error err) }, Cmd.none)

    | LoadStoryItem (_, Ok item) ->
        match state.Stories with
        | Resolved (Ok map) -> 
            let newMap =
                map
                |> Map.remove item.id
                |> Map.add item.id (Resolved (Ok item))

            ({ state with Stories = Resolved (Ok newMap) }, Cmd.none)

        | _ -> state, Cmd.none

    | LoadStoryItem (id, Error err) ->
        match state.Stories with
        | Resolved (Ok map) -> 
            let newMap =
                map
                |> Map.remove id
                |> Map.add id (Resolved (Error err))

            ({ state with Stories = Resolved (Ok newMap) }, Cmd.none)

        | _ -> state, Cmd.none

    | ChangeTab tab -> 
        ({ state with ActiveTab = tab; Stories = InProgress }, Cmd.fromAsync (loadStories state.ActiveTab))


let renderError (msg : string) =
    Html.h1 [
        prop.style [ style.color.red ]
        prop.text msg
    ]

let spinner =
    Html.div [
        prop.style [ style.textAlign.center; style.marginTop 20 ]
        prop.children [
            Html.i [
                prop.classes [ FA.Fa; FA.FaCog; FA.FaSpin; FA.Fa2X ]
            ]
        ]
    ]

let renderItemContent (story : Story) =
    match story.url with
    | Some url ->
        Html.a [
            prop.style [ style.textDecoration.underline ]
            prop.target.blank
            prop.href url
            prop.text story.title
        ]
    | None ->
        Html.p story.title

let renderItem (id : int) item =
    let renderedItem = 
        match item with
        | NotStartedYet -> Html.none
        | InProgress -> spinner
        | Resolved (Error msg) -> renderError msg
        | Resolved (Ok story) -> renderItemContent story

    Html.div [
        prop.key id
        prop.className B.Box
        prop.style [ style.marginTop 15; style.marginBottom 15 ]
        prop.children [ renderedItem ]
    ]


let renderItems items = 
    match items with
    | NotStartedYet -> Html.none
    | InProgress -> spinner
    | Resolved (Error msg) -> renderError msg
    | Resolved (Ok items) -> 
        items
        |> Map.toList
        |> List.map (fun (id, story) -> renderItem id story)
        |> Html.div

let renderTabs activeTab dispatch =
    let changeStories stories =
        if activeTab <> stories
        then dispatch (ChangeTab stories)
    
    let categoryName category =
        match category with
        | Tab.Top -> "Top"
        | Tab.Best -> "Best"
        | Tab.New -> "New"
        | Tab.Jobs -> "Jobs"

    Html.div [
        prop.classes [ B.Tabs; B.IsToggle; B.IsFullwidth ]
        prop.children [
            Html.ul [
                for stories in [ Tab.New; Tab.Top; Tab.Best; Tab.Jobs ] -> 
                Html.li [
                    prop.classes [ if activeTab = stories then B.IsActive ]
                    prop.onClick (fun _ -> changeStories stories)
                    prop.children [
                        Html.a [ 
                            Html.span [
                                prop.text (categoryName stories)
                            ]
                         ]
                    ]
                ]
            ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.style [ style.padding 20 ]
        prop.children [
            Html.h1 [
                prop.className B.Title
                prop.text "Elmish Hackernews"
            ]
            renderTabs state.ActiveTab dispatch
            renderItems state.Stories
        ]
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run