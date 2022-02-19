[<AutoOpen>]
module Extensions

open Elmish

module Cmd =
    let fromAsync (operation: Async<'msg>) : Cmd<'msg> =
        let delayedCmd (dispatch: 'msg -> unit) : unit =
            let delayedDispatch = async {
                let! msg = operation
                dispatch msg
            }

            Async.StartImmediate delayedDispatch

        Cmd.ofSub delayedCmd

module Async =
    let map f x =
        async {
            let! unwrapped = x
            return f unwrapped
        }


type Deferred<'t> =
    | NotStartedYet
    | InProgress
    | Resolved of 't

type AsyncEvent<'t> =
    | Started
    | Finished of 't