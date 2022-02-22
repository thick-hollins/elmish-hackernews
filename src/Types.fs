[<AutoOpen>]
module Types

type Deferred<'t> =
    | NotStartedYet
    | InProgress
    | Resolved of 't

type AsyncEvent<'t> =
    | Started
    | Finished of 't

[<RequireQualifiedAccess>]
type Tab = | New | Top | Best | Jobs

type Story =
    {
        id: int
        title: string
        url: string option
        score: int
        time: int
    }

type DeferredStory = Deferred<Result<Story, string>>

type State =
    { 
        Stories: Deferred<Result<Map<int, DeferredStory>, string>> 
        ActiveTab: Tab
    }

type Msg =
    | LoadStoryIds of AsyncEvent<Result<int list, string>>
    | LoadStoryItem of int * Result<Story, string> 
    | ChangeTab of Tab