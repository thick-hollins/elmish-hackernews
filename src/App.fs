module App

open Elmish
open Elmish.React
open Feliz
open Fable.SimpleHttp
open Thoth.Json
open Fable.DateFunctions
open System

let storyDecoder : Decoder<Story> =
    Decode.object (fun fields -> 
        {
            id = fields.Required.At [ "id" ] Decode.int
            title = fields.Required.At [ "title" ] Decode.string
            url = fields.Optional.At [ "url" ] Decode.string  
            score = fields.Required.At [ "score" ] Decode.int  
            time = fields.Required.At [ "time" ] Decode.int  
        }
    )

let loadStory (id : int) = 
    async {
        let storyUrl = sprintf "https://hacker-news.firebaseio.com/v0/item/%d.json" id
        let! (status, responseText) = Http.get storyUrl
        match status with
        | 200 -> 
            match Decode.fromString storyDecoder responseText with
            | Ok story -> return LoadStoryItem (id, Ok story)
            | Error parseError -> return LoadStoryItem (id, Error parseError)
        | _ -> return LoadStoryItem (id, Error ("Http loading error" + string id))
    }

let loadStories tab = 
    let category = 
        match tab with
        | Tab.New -> "new"
        | Tab.Top -> "top"
        | Tab.Best -> "best"
        | Tab.Jobs -> "job"

    let storiesEndpoint = sprintf "https://hacker-news.firebaseio.com/v0/%sstories.json" category
    
    async {
        let! (status, responseText) = Http.get storiesEndpoint
        match status with
        | 200 -> 
            let storyIds = Decode.fromString (Decode.list Decode.int) responseText
            match storyIds with
            | Ok ids -> 
                return LoadStoryIds (Finished (Ok ids))

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
            Queued = []
            Page = 1
        }

    newState, Cmd.ofMsg (LoadStoryIds Started)


let update (msg: Msg) (state: State) =
    match msg with
    | LoadStoryIds Started ->
        ({ state with Stories = InProgress }, Cmd.fromAsync (loadStories state.ActiveTab))

    | LoadStoryIds (Finished (Ok ids)) ->
        let load = List.truncate 10 ids
        let queued = List.skip 10 ids
        let map = Map.ofList [ for id in load -> (id, InProgress) ]
        let commands = [ for id in load -> Cmd.fromAsync (loadStory id) ]
        ({ state with Stories = Resolved (Ok map); Queued = queued }, Cmd.batch commands)

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

    | LoadMore ->
        match state.Stories with
        | Resolved (Ok stories) ->
            let load = List.truncate 10 state.Queued
            let queued = List.skip 10 state.Queued
            let map = Map.ofList [ for id in load -> (id, InProgress) ]
            let newMap = Map.fold (fun a k v -> Map.add k v a) stories map
            let commands = [ for id in load -> Cmd.fromAsync (loadStory id) ]
            ({ state with Stories = Resolved (Ok newMap); Queued = queued }, Cmd.batch commands)
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
    
    let fromUnix seconds =
        DateTime(1970, 1, 1, 0, 0, 0, 0).AddSeconds(seconds) 
    let ago = DateTime.UtcNow.FormatDistance(fromUnix(float story.time))

    Html.div [
        prop.classes [ B.Columns; B.IsMobile ]
        prop.children [
            Html.div [
                prop.classes [ B.Column; B.IsNarrow ]
                prop.children [
                    Html.div [
                        prop.className B.Icon
                        prop.style [ style.marginLeft 20 ]
                        prop.children [
                            Html.i [prop.className "fa fa-poll fa-2x"]
                            Html.span [
                                prop.style [ style.marginLeft 10; style.marginRight 10 ]
                                prop.text story.score
                            ]
                        ]
                    ]
                ]
            ]
            Html.div [
                prop.className B.Column
                prop.children [
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
                    Html.p [
                        prop.text (sprintf "%s ago" ago)
                    ]
                ]
            ]
        ]
    ]

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
        |> List.sortByDescending (fun (id, story) -> 
            match story with
            | Resolved (Ok story) -> story.time
            | _ -> 0 )
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
            Html.button [
                prop.text "Load More"
                prop.onClick (fun _ -> dispatch LoadMore)
            ]
        ]
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run