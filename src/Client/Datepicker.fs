module RoosterStartdatumPicker

open System
open Elmish
open Elmish.React
open Fable.React.Props
open Fable.React

open Calendar
open Client

type ViewState = Open | Closed

type State = {
    SubmitName : string
    Label: string
    IsOpen : ViewState
    UserInput : string
    SelectedDate : DateTime
    Calendar : Calendar.State
}

type Msg =
    | ToggleDatePicker
    | UserInput of string
    | CalendarMessages of Calendar.Msg

let init (date : string) (label : string) (submitname : string) () =
    let (date, userinput) =
        match DateTime.TryParse(date) with
        | true, date -> date, date.ToString("dd/MM/yyyy")
        | false, _ -> DateTime.Today, ""

    let label = match label with | "" | null -> "Datum" | _ -> label
    let submitname = match submitname with | "" | null -> "Datum" | _ -> submitname

    {
        SubmitName = submitname
        Label = label
        IsOpen = Closed
        UserInput = userinput
        SelectedDate = date
        Calendar = fst (Calendar.single date)
    }, Cmd.none


let update msg state =
    match msg with
    | UserInput value ->
        { state with UserInput = value }, Cmd.none
    | ToggleDatePicker ->
         match state.IsOpen with
         | Open ->  { state with IsOpen =  Closed}, Cmd.none
         | Closed ->  { state with IsOpen =  Open}, Cmd.none
    | CalendarMessages x ->
        let nextCalendarState, nextCalenderCmd = Calendar.update x state.Calendar

        let updatedOwnState =
            match x with
            | SelectDate dt -> { state with SelectedDate = dt; IsOpen = Closed; UserInput = dt.ToString("dd/MM/yyyy") }
            | _ -> state

        { updatedOwnState with Calendar = nextCalendarState }, (Cmd.map CalendarMessages nextCalenderCmd)

let view (state : State) (dispatch : Msg -> unit) =
    div [ Style [ Width "100%" ] ] [
        div [ Class "a-input has-icon-right" ] [
            yield label [ Class "a-input__label" ] [ str state.Label ]
            yield div [
                Class "a-input__wrapper"
            ] [
                yield ReactMask.create [
                    ReactMask.MaskOptions.Value state.UserInput
                    ReactMask.MaskOptions.OnChange (fun e -> dispatch <| UserInput e.Value)
                    ReactMask.MaskOptions.Name state.SubmitName
                    ReactMask.MaskOptions.Mask "99/99/9999"
                ]
                yield span [
                    OnClick (fun _ -> dispatch ToggleDatePicker)
                    Class "fa fa-calendar is-clickable"
                ] []

                match state.IsOpen with
                | Open -> yield Calendar.view Calendar.Position.Bottom state.Calendar (CalendarMessages >> dispatch)
                | Closed -> yield null
            ]
        ]
    ]


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

let container = Browser.Dom.document.getElementById "datepicker"
let date = container.getAttribute "data-date"
let label = container.getAttribute "data-label"
let submit = container.getAttribute "data-submit-name"


Program.mkProgram (init date label submit) update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "datepicker"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
