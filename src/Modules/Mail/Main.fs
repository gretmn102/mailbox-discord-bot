module Mail.Main
open DSharpPlus
open FsharpMyExtension
open FsharpMyExtension.Either

open Types
open Extensions
open Model

type State =
    {
        Mails: Mails.MailDb
        UserEditStates: UserEditStates.GuildData
    }

type EditAction =
    | Display
    | To of UserId
    | From of string
    | Description of string
    | Return
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EditAction =
    module Parser =
        open FParsec

        open DiscordMessage.Parser

        type 'Result Parser = Primitives.Parser<'Result, unit>

        module CommandNames =
            let display = "отобразить"
            let ``to`` = "кому"
            let from = "от"
            let description = "описание"
            let returnToMails = "вернуться"

        let pdisplay: _ Parser =
            let p =
                skipStringCI CommandNames.display

            p >>% Display

        let pto: _ Parser =
            let p =
                skipStringCI CommandNames.``to`` .>> spaces
                >>. (puserMention <|> puint64)

            p |>> To

        let pfrom: _ Parser =
            let p =
                skipStringCI CommandNames.from .>> spaces
                >>. many1Satisfy (fun _ -> true)

            p |>> From

        let pdescription: _ Parser =
            let p =
                skipStringCI CommandNames.description .>> spaces
                >>. manySatisfy (fun _ -> true)

            p |>> Description

        let preturnToMails: _ Parser =
            let p =
                skipStringCI CommandNames.returnToMails

            p >>% Return

        let start: _ Parser =
            choice [
                pdisplay
                pto
                pfrom
                pdescription
                preturnToMails
            ]

type MainAction =
    | CreateMail of UserId
    | DisplayMails
    | EditMail of MailIndex
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MainAction =
    module Parser =
        open FParsec

        open DiscordMessage.Parser

        type 'Result Parser = Primitives.Parser<'Result, unit>

        module CommandNames =
            let displayMails = "письма"
            let createMail = "создать"
            let editMail = "редактировать"

        let pdisplayMails: _ Parser =
            let p =
                skipStringCI CommandNames.displayMails

            p >>% DisplayMails

        let pcreateMail: _ Parser =
            let p =
                skipStringCI CommandNames.createMail .>> spaces
                >>. (puserMention <|> puint64)

            p |>> CreateMail

        let peditMail: _ Parser =
            let p =
                skipStringCI CommandNames.editMail .>> spaces
                >>. pint32

            p |>> EditMail

        let start: _ Parser =
            choice [
                pdisplayMails
                pcreateMail
                peditMail
            ]

type Action =
    | EditAction of EditAction * MailId
    | MainAction of MainAction

type Msg =
    | Request of DiscordClient * EventArgs.MessageCreateEventArgs

let reduce (msg: Msg) (state: State): State =
    match msg with
    | Request(client, e) ->
        let send msg =
            let embed = Entities.DiscordEmbedBuilder()
            embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
            embed.Description <- msg

            let b = Entities.DiscordMessageBuilder()
            b.Embed <- embed.Build()

            awaiti <| e.Channel.SendMessageAsync b

        let checkIsPrivate next =
            let currentUserId = client.CurrentUser.Id

            if e.Author.Id = currentUserId then
                state
            else
                if e.Channel.IsPrivate then
                    next ()
                else
                    if e.Message.MentionedUsers |> Seq.exists (fun user -> user.Id = currentUserId) then
                        sprintf "Напиши мне в ЛС любое сообщение."
                        |> send

                    state

        let getUserEditState next =
            UserEditStates.GuildData.tryFindById e.Author.Id state.UserEditStates
            |> next

        let checkFirstTime (userStateOpt: UserEditStates.Data option) next =
            match userStateOpt with
            | None ->
                let userEditStates =
                    state.UserEditStates
                    |> UserEditStates.GuildData.set
                        e.Author.Id
                        id

                [
                    "Хо-хо-хо! Я — бот, рассылающий поздравления людям под Новый год! <:satan:1044024010113032212>"
                    sprintf "Введи `%s <id_пользователя>`, чтобы начать писать поздравление указанному пользователю. Например:" MainAction.Parser.CommandNames.createMail
                    "```"
                    sprintf "%s %d" MainAction.Parser.CommandNames.createMail client.CurrentUser.Id
                    "```"
                    "Начнешь писать письмо мне, но я все равно его не оценю, поэтому лучше направить свои силы на кого-то другого. После того, как напишешь, следуй моим указаниям."
                ]
                |> String.concat "\n"
                |> send

                { state with
                    UserEditStates = userEditStates
                }

            | Some userState ->
                next userState

        let mainCommands =
            [
                "Доступные команды:"
                sprintf "• `%s <id_пользователя>` — начать писать поздравление указанному пользователю;" MainAction.Parser.CommandNames.createMail
                sprintf "• `%s` — отображает список написанных писем;" MainAction.Parser.CommandNames.displayMails
                sprintf "• `%s <индекс_письма>` — начать редактировать письмо с указанным индексом. Посмотреть индексы писем можно командой `%s`" MainAction.Parser.CommandNames.editMail MainAction.Parser.CommandNames.displayMails
            ]
            |> String.concat "\n"

        let editCommands =
            [
                sprintf "Ты находишься в режиме редактирования письма. Доступные команды:"
                sprintf "• `%s` — отображает письмо в том виде, которое получит указанный пользователь" EditAction.Parser.CommandNames.display
                sprintf "• `%s <id_пользователя>` — сменить получателя, если вдруг ошиблись с ID" EditAction.Parser.CommandNames.``to``
                sprintf "• `%s <произвольный_набор_символов>` — указывает отрпавителя. По умолчанию указан \"Аноним\", но ты можешь подписаться как угодно" EditAction.Parser.CommandNames.from
                sprintf "• `%s <произвольный_набор_символов>` — указывает содержимое сообщения" EditAction.Parser.CommandNames.description
                sprintf "• `%s` — вернуться в главное меню, чтобы написать кучу новых поздравлений, либо же подредактировать существующие!" EditAction.Parser.CommandNames.returnToMails
            ]
            |> String.concat "\n"

        let parseCommandByUserState (userState: UserEditStates.Data) next =
            match userState.Data.Editing with
            | Some mailId ->
                match FParsecExt.runResult EditAction.Parser.start e.Message.Content with
                | Ok act ->
                    next (EditAction(act, mailId))
                | Error msg ->
                    editCommands
                    |> send

                    state
            | None ->
                match FParsecExt.runResult MainAction.Parser.start e.Message.Content with
                | Ok act ->
                    next (MainAction act)
                | Error msg ->
                    mainCommands
                    |> send

                    state

        checkIsPrivate <| fun () ->
        getUserEditState <| fun userStateOpt ->
        checkFirstTime userStateOpt <| fun userState ->
        parseCommandByUserState userState <| fun command ->

        let displayMail (mail: Mails.Mail) =
            let embed = Entities.DiscordEmbedBuilder()

            embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
            embed.Description <- mail.Data.Description
            embed.WithAuthor(mail.Data.From) |> ignore

            let b = Entities.DiscordMessageBuilder()
            b.Content <-
                let recipientId = mail.Data.Recipient
                sprintf "<@%d>(%d) получит такое сообщение:" recipientId recipientId
            b.Embed <- embed.Build()

            awaiti <| e.Channel.SendMessageAsync b

        match command with
        | EditAction(act, mailId) ->
            let getMailById (mailId: MailId) next =
                match Mails.MailDb.tryFindById mailId state.Mails with
                | Some mail ->
                    next mail
                | None ->
                    sprintf "Письмо удалилось по какой-то причине. Начните заново."
                    |> send

                    state

            getMailById mailId <| fun mail ->

            let update updating =
                let newData =
                    updating mail.Data

                let mails =
                    state.Mails
                    |> Mails.MailDb.set
                        mail.Id
                        (fun _ ->
                            newData
                        )

                displayMail { mail with Data = newData }

                { state with
                    Mails = mails
                }

            match act with
            | Display ->
                displayMail mail
                state
            | To ``to`` ->
                update (fun mailData ->
                    { mailData with
                        Recipient = ``to``
                    }
                )
            | From from ->
                update (fun mailData ->
                    { mailData with
                        From = from
                    }
                )
            | Description description ->
                update (fun mailData ->
                    { mailData with
                        Description = description
                    }
                )
            | Return ->
                let userEditStates =
                    state.UserEditStates
                    |> UserEditStates.GuildData.set
                        e.Author.Id
                        (fun x ->
                            { x with
                                Editing = None
                            }
                        )

                mainCommands
                |> send

                { state with
                    UserEditStates = userEditStates
                }

        | MainAction(act) ->
            match act with
            | EditMail mailIndex ->
                let getMailByIndex (mailIndex: MailIndex) next =
                    match Mails.MailDb.tryFindByMailIndex e.Author.Id mailIndex state.Mails with
                    | Some mail ->
                        next mail
                    | None ->
                        sprintf "Письмо под номером %d не найдено. Попробуйте еще раз вызвать список писем командой `%s` и указать номер нужного письма для редактирования."
                            mailIndex
                            MainAction.Parser.CommandNames.displayMails
                        |> send

                        state

                getMailByIndex mailIndex <| fun mail ->

                let userEditStates =
                    state.UserEditStates
                    |> UserEditStates.GuildData.set
                        e.Author.Id
                        (fun x ->
                            { x with
                                Editing = Some mail.Id
                            }
                        )

                editCommands
                |> send

                displayMail mail

                { state with
                    UserEditStates = userEditStates
                }
            | CreateMail recipientId ->
                let newMailId =
                    MailId.generateNew ()

                let mailData =
                    { Mails.MailData.Empty with
                        Author = e.Author.Id
                        Recipient = recipientId
                    }

                let mails =
                    state.Mails
                    |> Mails.MailDb.set
                        newMailId
                        (fun _ ->
                            mailData
                        )

                let userEditStates =
                    state.UserEditStates
                    |> UserEditStates.GuildData.set
                        e.Author.Id
                        (fun x ->
                            { x with
                                Editing = Some newMailId
                            }
                        )

                let mail =
                    Mails.Mail.create
                        newMailId
                        mailData

                editCommands
                |> send

                displayMail mail

                { state with
                    Mails = mails
                    UserEditStates = userEditStates
                }

            | DisplayMails ->
                let getMails next =
                    match Mails.MailDb.tryFindByUserId e.Author.Id state.Mails with
                    | Some mails ->
                        next mails
                    | None ->
                        "Список писем пуст."
                        |> send

                        state

                getMails <| fun mails ->

                mails
                |> List.mapi (fun i mailId -> sprintf "%d %s" i mailId) // TODO
                |> String.concat "\n"
                |> send

                state

let create db =
    let m =
        let init: State = {
            Mails = Mails.MailDb.init "mailboxUsers" db
            UserEditStates = UserEditStates.GuildData.init "mailboxUserEditStates" db
        }

        MailboxProcessor.Start (fun mail ->
            let rec loop (state: State) =
                async {
                    let! msg = mail.Receive()
                    let state =
                        try
                            reduce msg state
                        with e ->
                            printfn "%A" e
                            state

                    return! loop state
                }
            loop init
        )

    { Shared.BotModule.empty with
        MessageCreateEventHandle =
            let exec (e, msg) =
                m.Post (Request (e, msg))
            Some exec
    }
