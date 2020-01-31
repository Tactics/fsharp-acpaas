module Datepicker

open System
open Elmish
open Fable.React.Props
open Fable.React
open Calendar

type ViewState = Open | Closed

type State = {
    IsOpen : ViewState
    SelectedDate : DateTime option
    Calendar : Calendar.State
}

type Msg = | ToggleDatePicker | ClearDate | CalendarMessages of Calendar.Msg

let init (dt : DateTime option) =
    {
        IsOpen = Closed
        SelectedDate = dt
        Calendar = fst (Calendar.single (dt |> Option.defaultValue DateTime.Today))
    }, Cmd.none 

let update msg state =
    match msg with
    | ToggleDatePicker ->
         match state.IsOpen with
         | Open ->  { state with IsOpen =  Closed}, Cmd.none
         | Closed ->  { state with IsOpen =  Open}, Cmd.none
    | ClearDate ->
         { state with SelectedDate = None; }, Cmd.none
    | CalendarMessages x ->
        let nextCalendarState, nextCalenderCmd = Calendar.update x state.Calendar
        
        let updatedOwnState = 
            match x with
            | SelectDate dt -> { state with SelectedDate = Some dt; IsOpen = Closed; }
            | _ -> state
        
        { updatedOwnState with Calendar = nextCalendarState }, (Cmd.map CalendarMessages nextCalenderCmd)
   
let view (position : Position) (state : State) (dispatch : Msg -> unit) =                               
    div [ Style [ Width "100%" ] ] [
        div [ Class "a-input a-input--small has-icon-right" ] [
            div [
                Class "a-input__wrapper"
            ] [
                yield input [
                    OnChange (fun _ -> dispatch ClearDate)
                    OnClick (fun _ -> dispatch ToggleDatePicker)
                    Value (match state.SelectedDate with Some date -> date.ToString("dd/MM/yyyy") | None -> "")
                ]
                yield span [
                    OnClick (fun _ -> dispatch ToggleDatePicker)
                    Class "fa fa-calendar is-clickable"
                ] []
                
                match state.IsOpen with
                | Open -> yield Calendar.view position state.Calendar (CalendarMessages >> dispatch)
                | Closed -> yield span [] []
            ]
        ]                
    ]