module Calendar

open System
open Elmish
open Fable.React.Props
open Fable.React

type ClassMap = (string * bool) list

module ClassMap =
    let private concat s1 s2 = sprintf "%s %s" s1 s2
    let private onlyClassesThatApply = snd
    let private classname = fst

    let toString (map : ClassMap) : string =
        map
        |> List.filter onlyClassesThatApply
        |> List.map classname
        |> List.fold concat ""


type CurrentSelection = Days | Months | Years

type Maand = int
module Maand =
    let toString (x : int) =
        match x with
        | 1 -> "januari"
        | 2 -> "februari"
        | 3 -> "maart"
        | 4 -> "april"
        | 5 -> "mei"
        | 6 -> "juni"
        | 7 -> "juli"
        | 8 -> "augustus"
        | 9 -> "september"
        | 10 -> "oktober"
        | 11 -> "november"
        | 12 -> "december"
        | _ -> failwith (sprintf "this is not a valid month %s" (string x))

type DateSelector =
    | Single of DateTime
    | Multiple of DateTime list

type Position = Top | Bottom

type State = {
    Dates : DateSelector
    MinDate : DateTime option
    MaxDate : DateTime option
    CurrentSelection : CurrentSelection
    CurrentMonth : Maand
    CurrentYear : int
}

type Msg =
    | ToggleSelection
    | ChangeSelection of CurrentSelection
    | ChangeMonth of Maand
    | ChangeYear of int
    | NavigateMonth of Maand
    | NavigateYear of int
    | SelectDate of DateTime

let single (dt : DateTime) =
    {
        Dates = Single dt.Date
        MinDate = None
        MaxDate = None
        CurrentSelection = Days
        CurrentMonth = dt.Month
        CurrentYear = dt.Year
    }, Cmd.none

let multi (dt : DateTime) =
    {
        Dates = Multiple []
        MinDate = None
        MaxDate = None
        CurrentSelection = Days
        CurrentMonth = dt.Month
        CurrentYear = dt.Year
    }, Cmd.none


let update msg state =
    match msg with
    | ToggleSelection ->
        match state.CurrentSelection with
        | Days -> { state with CurrentSelection =  Months}, Cmd.none
        | Months -> { state with CurrentSelection =  Years}, Cmd.none
        | Years -> { state with CurrentSelection =  Days}, Cmd.none
    | ChangeMonth m ->
        { state with CurrentMonth = m }, Cmd.ofMsg <| ChangeSelection Days
    | ChangeYear y ->
        { state with CurrentYear = y }, Cmd.ofMsg <| ChangeSelection Months
    | ChangeSelection s ->
        { state with CurrentSelection =  s}, Cmd.none
    | NavigateMonth m ->
        match m with
        | 0 -> { state with CurrentMonth = 12; CurrentYear = state.CurrentYear - 1 }, Cmd.none
        | 13 -> { state with CurrentMonth = 1; CurrentYear = state.CurrentYear + 1 }, Cmd.none
        | _ -> { state with CurrentMonth = m }, Cmd.none
    | NavigateYear y ->
        { state with CurrentYear = y }, Cmd.none
    | SelectDate dt ->
        match state.Dates with
        | Single _ ->
            { state with
                Dates = Single dt
                CurrentMonth = dt.Month
                CurrentYear = dt.Year
            }, Cmd.none
        | Multiple dates ->
            match List.contains dt dates with
            | true -> { state with Dates = Multiple <| List.filter (fun d -> d.Date <> dt.Date) dates }, Cmd.none
            | false -> { state with Dates = Multiple <| dt :: dates }, Cmd.none


let view (position : Position)(state : State) (dispatch : Msg -> unit) =
    let header (currentMonth : int) (currentYear : int) currentSelection=
        let title =
            match currentSelection with
            | Days -> Maand.toString currentMonth + " " + (string currentYear)
            | Months -> string currentYear
            | Years -> string currentYear + " - " + string (currentYear + 11)

        let prevActionMsg =
            match currentSelection with
            | Days -> NavigateMonth (currentMonth - 1)
            | Months -> NavigateYear (currentYear - 1)
            | Years -> NavigateYear (currentYear - 12)

        let nextActionMsg =
            match currentSelection with
            | Days -> NavigateMonth (currentMonth + 1)
            | Months -> NavigateYear (currentYear + 1)
            | Years -> NavigateYear (currentYear + 12)

        div [ Class "m-datepicker__nav" ] [
            button [
                TabIndex -1; Class "a-button has-icon"
                OnClick (fun e -> e.preventDefault(); dispatch prevActionMsg)

            ] [ i [ Class "fa fa-angle-left" ][] ]
            button [
                TabIndex 0; Class "m-datepicker__title a-button"
                OnClick (fun e -> e.preventDefault(); dispatch ToggleSelection)
            ] [ str <| title ]
            button [
                TabIndex 0; Class "a-button has-icon"
                OnClick (fun e -> e.preventDefault(); dispatch nextActionMsg)
            ] [ i [ Class "fa fa-angle-right" ][] ]
        ]

    let body (currentMonth : int) (currentYear : int) currentSelection (dateselector : DateSelector) (min : DateTime option) (max : DateTime option) =
        match currentSelection with
            | Days ->
                let firstOfMonth = (new DateTime(currentYear, currentMonth, 1)).ToLocalTime()
                let lastOfMonth = firstOfMonth.AddMonths(1).AddDays(-1.)

                let firstOfCalendar =
                    match (int) firstOfMonth.DayOfWeek with
                    | 1 -> firstOfMonth
                    | 0 -> firstOfMonth.AddDays(-6.)
                    | i -> firstOfMonth.AddDays(-(float (i-1)))

                let lastOfCalendar =
                    match (int) lastOfMonth.DayOfWeek with
                    | 0 -> lastOfMonth
                    | 1 -> lastOfMonth.AddDays(6.)
                    | i -> lastOfMonth.AddDays((float (7-i)))


                let days = Seq.unfold (fun (d : DateTime) -> if d.Date <= lastOfCalendar.Date then Some(d, new DateTime(d.Year, d.Month, d.Day+1)) else None) firstOfCalendar
                let daysInCalendar = days |> Seq.chunkBySize 7 |> List.ofSeq |> List.filter (fun dts -> dts.Length = 7)

                table [] [
                    thead [] [
                        tr [ Class "m-datepicker__days" ] [
                            th [ Class "u-text-center" ] [ str "Ma" ]
                            th [ Class "u-text-center" ] [ str "Di" ]
                            th [ Class "u-text-center" ] [ str "Wo" ]
                            th [ Class "u-text-center" ] [ str "Do" ]
                            th [ Class "u-text-center" ] [ str "Vr" ]
                            th [ Class "u-text-center" ] [ str "Za" ]
                            th [ Class "u-text-center" ] [ str "Zo" ]
                        ]
                    ]
                    tbody [ Class "m-datepicker__calendar" ] [
                       daysInCalendar |> List.map (fun dts -> tr [ Key (string (Guid.NewGuid())) ] [
                           dts |> List.ofArray |> List.map ( fun dt ->

                               let canSet (dt : DateTime) =
                                   match min, max with
                                   | None, None -> true
                                   | Some minDt, Some maxDt -> if (dt >= minDt && dt <= maxDt) then true else false
                                   | Some minDt, None -> if (dt >= minDt) then true else false
                                   | None, Some maxDt -> if (dt <= maxDt) then true else false

                               let isSelected =
                                   match dateselector with
                                   | Single date -> dt = date
                                   | Multiple dates -> List.contains dt dates


                               let dayClass = [
                                   ("is-faded", currentMonth <> dt.Month )
                                   ("is-unavailable", not (canSet dt))
                                   ("is-selected", isSelected)
                               ]

                               td [ Key (dt.ToString("dd-MM-yyy")) ] [
                                   button [
                                       yield Class <| ClassMap.toString dayClass
                                       if (canSet dt) then
                                           yield OnClick (fun e -> e.preventDefault(); dispatch <| SelectDate dt)
                                       else
                                           yield OnClick (fun _ -> ())
                                   ] [ str (string dt.Day) ]
                               ]
                            ) |> ofList
                       ]) |> ofList
                    ]
                ]

            | Years ->
                table [] [
                    tbody [ Class "m-datepicker__calendar" ] [
                        tr [] [
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeYear currentYear) ] [ str (string currentYear) ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeYear (currentYear + 1)) ] [ str (string <| currentYear + 1) ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeYear (currentYear + 2)) ] [ str (string <|currentYear + 2) ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeYear (currentYear + 3)) ] [ str (string <| currentYear + 3) ]]
                        ]
                        tr [] [
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeYear (currentYear + 4)) ] [ str (string <| currentYear + 4) ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeYear (currentYear + 5)) ] [ str (string <| currentYear + 5) ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeYear (currentYear + 6)) ] [ str (string <| currentYear + 6) ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeYear (currentYear + 7)) ] [ str (string <| currentYear + 7) ]]
                        ]
                        tr [] [
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeYear (currentYear + 8)) ] [ str (string <| currentYear + 8) ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeYear (currentYear + 9)) ] [ str (string <| currentYear + 9) ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeYear (currentYear + 10)) ] [ str (string <| currentYear + 10) ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeYear (currentYear + 11)) ] [ str (string <| currentYear + 11) ]]
                        ]
                    ]
                ]
            | Months ->
                table [] [
                    tbody [ Class "m-datepicker__calendar" ] [
                        tr [] [
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeMonth 1) ] [ str "Januari" ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeMonth 2) ] [ str "Februari" ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeMonth 3) ] [ str "Maart" ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeMonth 4) ] [ str "April" ]]
                        ]
                        tr [] [
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeMonth 5) ] [str "Mei" ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeMonth 6) ] [str "Juni" ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeMonth 7) ] [str "Juli" ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeMonth 8) ] [str "Augustus" ]]
                        ]
                        tr [] [
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeMonth 9) ] [str "September" ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeMonth 10) ] [str "Oktober" ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeMonth 11) ] [str "November" ]]
                            td [] [ button [ OnClick (fun e -> e.preventDefault(); dispatch <| ChangeMonth 12) ] [str "December" ]]
                        ]
                    ]
                ]

    let classes = [
        ("m-datepicker", true)
        ("m-datepicker--fixed", match state.Dates with | Multiple _ -> false | Single _ -> true)
        ("top", position = Top)
        ("is-open", true)
    ]
    div [ Class "a-input__wrapper" ] [
        div [ Role "datepicker"; Class <| ClassMap.toString classes; ] [
            header state.CurrentMonth state.CurrentYear state.CurrentSelection
            body state.CurrentMonth state.CurrentYear state.CurrentSelection state.Dates state.MinDate state.MaxDate
        ]
    ]