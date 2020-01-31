module Client.ReactMask

open Browser.Types
open Fable.Core
open Fable.React

    type MaskOptions =
        | Mask of string
        | Name of string
        | Value of string
        | OnChange of (MouseEvent -> unit)

    let private internalCreate (_options: obj) = JsInterop.importDefault "./ReactMask.js"

    let create (options : MaskOptions list) : ReactElement =
        printfn "%A" options
        internalCreate (JsInterop.keyValueList CaseRules.LowerFirst options)

