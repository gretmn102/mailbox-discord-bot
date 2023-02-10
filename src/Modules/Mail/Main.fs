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

[<Struct>]
type PostmanType =
    | SantaClaus
    | Valentine
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module PostmanType =
    let tryDeserialize = function
        | "SantaClaus" -> Ok SantaClaus
        | "Valentine" -> Ok Valentine
        | unknown ->
            let values =
                Reflection.Reflection.unionEnum<PostmanType>
            sprintf "Unknown '%s' postman type. Available values: %A" unknown values
            |> Error

type EditAction =
    | Display
    | To of UserId
    | From of string
    | Description of string
    | ImageUrl of string
    | Return
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EditAction =
    module CommandNames =
        let display = "отобразить"
        let ``to`` = "кому"
        let from = "от"
        let description = "описание"
        let returnToMails = "вернуться"
        let image = "картинка"

    let help postmanType =
        match postmanType with
        | SantaClaus ->
            [
                sprintf "Ты находишься в режиме редактирования письма. Доступные команды:"
                sprintf "• `%s` — отображает письмо в том виде, которое получит указанный пользователь" CommandNames.display
                sprintf "• `%s <id_пользователя>` — сменить получателя, если вдруг ошиблись с ID" CommandNames.``to``
                sprintf "• `%s <произвольный_набор_символов>` — указывает отправителя. По умолчанию указан \"Аноним\", но ты можешь подписаться как угодно" CommandNames.from
                sprintf "• `%s <произвольный_набор_символов>` — указывает содержимое сообщения" CommandNames.description
                sprintf "• `%s [<ссылка на картинку>]` — добавляет внизу картинку. Чтобы убрать, напишите команду без аргументов" CommandNames.image
                sprintf "• `%s` — вернуться в главное меню, чтобы написать кучу новых поздравлений, либо же подредактировать существующие!" CommandNames.returnToMails
            ]
            |> String.concat "\n"
        | Valentine ->
            [
                sprintf "Ты находишься в режиме редактирования валентинки. Доступные команды:"
                sprintf "• `%s` — отображает валентинку в том виде, которое получит указанный пользователь" CommandNames.display
                sprintf "• `%s <id_пользователя>` — сменить получателя, если вдруг ошиблись с ID" CommandNames.``to``
                sprintf "• `%s <произвольный_набор_символов>` — указывает отправителя. По умолчанию указан \"Аноним\", но ты можешь подписаться как угодно" CommandNames.from
                sprintf "• `%s <произвольный_набор_символов>` — указывает содержимое сообщения" CommandNames.description
                sprintf "• `%s [<ссылка на картинку>]` — добавляет внизу картинку. Чтобы убрать, напишите команду без аргументов" CommandNames.image
                sprintf "• `%s` — вернуться в главное меню, чтобы написать кучу новых валентинок, либо же подредактировать существующие!" CommandNames.returnToMails
            ]
            |> String.concat "\n"

    module Parser =
        open FParsec

        open DiscordMessage.Parser

        type 'Result Parser = Primitives.Parser<'Result, unit>

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

        let pimage: _ Parser =
            let p =
                skipStringCI CommandNames.image .>> spaces
                >>. manySatisfy (fun _ -> true)

            p |>> ImageUrl

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
                pimage
                preturnToMails
            ]

    let handle postmanType send displayMail authorId mailId act state =
        let getMailById (mailId: MailId) next =
            match Mails.MailDb.tryFindById mailId state.Mails with
            | Some mail ->
                next mail
            | None ->
                match postmanType with
                | SantaClaus ->
                    sprintf "Письмо удалилось по какой-то причине. Начните заново."
                | Valentine ->
                    sprintf "Валентинка удалилась по какой-то причине. Начните заново."
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
        | ImageUrl imageUrl ->
            update (fun mailData ->
                { mailData with
                    ImageUrl = imageUrl
                }
            )
        | Return ->
            let userEditStates =
                state.UserEditStates
                |> UserEditStates.GuildData.set
                    authorId
                    (fun x ->
                        { x with
                            Editing = None
                        }
                    )

            help postmanType
            |> send

            { state with
                UserEditStates = userEditStates
            }

type MainAction =
    | CreateMail of UserId
    | DisplayMails
    | EditMail of MailIndex
    | RemoveMail of MailIndex
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MainAction =
    module CommandNames =
        let displayMails (postmanType: PostmanType) =
            match postmanType with
            | SantaClaus -> "письма"
            | Valentine -> "валентинки"
        let createMail = "создать"
        let editMail = "редактировать"
        let removeMail = "удалить"

    let help postmanType =
        let displayMailCommandName = CommandNames.displayMails postmanType

        match postmanType with
        | SantaClaus ->
            [
                "Доступные команды:"
                sprintf "• `%s <id_пользователя>` — начать писать поздравление указанному пользователю;" CommandNames.createMail
                sprintf "• `%s` — отображает список написанных писем;" displayMailCommandName
                sprintf "• `%s <индекс_письма>` — начать редактировать письмо с указанным индексом. Посмотреть индексы писем можно командой `%s` в графе `№`" CommandNames.editMail displayMailCommandName
                sprintf "• `%s <индекс_письма>` — удалить письмо с указанным индексом. Посмотреть индексы писем можно командой `%s` в графе `№`" CommandNames.removeMail displayMailCommandName
            ]
            |> String.concat "\n"
        | Valentine ->
            [
                "Доступные команды:"
                sprintf "• `%s <id_пользователя>` — начать писать валентинку указанному пользователю;" CommandNames.createMail
                sprintf "• `%s` — отображает список написанных валентинок;" displayMailCommandName
                sprintf "• `%s <индекс_валентинки>` — начать редактировать валентинку с указанным индексом. Посмотреть индексы валентинок можно командой `%s` в графе `№`" CommandNames.editMail displayMailCommandName
                sprintf "• `%s <индекс_валентинки>` — удалить валентинку с указанным индексом. Посмотреть индексы валентинок можно командой `%s` в графе `№`" CommandNames.removeMail displayMailCommandName
            ]
            |> String.concat "\n"

    module Parser =
        open FParsec

        open DiscordMessage.Parser

        type 'Result Parser = Primitives.Parser<'Result, unit>


        let pdisplayMails (postmanType: PostmanType): _ Parser =
            let p =
                skipStringCI (CommandNames.displayMails postmanType)

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

        let premoveMail: _ Parser =
            let p =
                skipStringCI CommandNames.removeMail .>> spaces
                >>. pint32

            p |>> RemoveMail

        let start (postmanType: PostmanType): _ Parser =
            choice [
                pdisplayMails postmanType
                pcreateMail
                peditMail
                premoveMail
            ]

type Action =
    | EditAction of EditAction * MailId
    | MainAction of MainAction

type Msg =
    | Request of DiscordClient * EventArgs.MessageCreateEventArgs

let reduce (postmanType: PostmanType) (msg: Msg) (state: State): State =
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

                let msg =
                    match postmanType with
                    | SantaClaus ->
                        [
                            "Хо-хо-хо! Я — бот, рассылающий поздравления людям под Новый год! <:satan:1044024010113032212>"
                            sprintf "Введи `%s <id_пользователя>`, чтобы начать писать поздравление указанному пользователю. Например:" MainAction.CommandNames.createMail
                            "```"
                            sprintf "%s %d" MainAction.CommandNames.createMail client.CurrentUser.Id
                            "```"
                            "Начнешь писать письмо мне, но я все равно его не оценю, поэтому лучше направить свои силы на кого-то другого. После того, как напишешь, следуй моим указаниям."
                        ]
                        |> String.concat "\n"
                    | Valentine ->
                        [
                            "П-привет! Сегодня мой первый рабочий день, и я немного волнуюсь, поэтому... Что, какой запах валерьянки?! Это одеколон! :scream_cat:"
                            "Ладно, о чем это я? Ах, да: день влюбленных! Пора романтики, грез и... валентинок. Я как раз такие собираю и доставляю другим половинкам."
                            sprintf "Введи `%s <id_пользователя>`, чтобы начать писать валентинку указанному пользователю." MainAction.CommandNames.createMail
                            ""
                            "Например, если написать так:"
                            "```"
                            sprintf "%s %d" MainAction.CommandNames.createMail client.CurrentUser.Id
                            "```"
                            "То эта валентика придет ко м-мне, воть."
                            "*Неловко мнется*"
                            ""
                            "После того, как напишешь, следуй дальнейшим указаниям, и всё будет хорошо 🐱"
                        ]
                        |> String.concat "\n"
                send msg

                { state with
                    UserEditStates = userEditStates
                }

            | Some userState ->
                next userState

        let parseCommandByUserState (userState: UserEditStates.Data) next =
            match userState.Data.Editing with
            | Some mailId ->
                match FParsecExt.runResult EditAction.Parser.start e.Message.Content with
                | Ok act ->
                    next (EditAction(act, mailId))
                | Error msg ->
                    EditAction.help postmanType
                    |> send

                    state
            | None ->
                match FParsecExt.runResult (MainAction.Parser.start postmanType) e.Message.Content with
                | Ok act ->
                    next (MainAction act)
                | Error msg ->
                    MainAction.help postmanType
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

            let imageUrl = mail.Data.ImageUrl
            if not <| System.String.IsNullOrEmpty imageUrl then
                embed.ImageUrl <- imageUrl

            let b = Entities.DiscordMessageBuilder()
            b.Content <-
                let recipientId = mail.Data.Recipient
                sprintf "<@%d>(%d) получит такое сообщение:" recipientId recipientId
            b.Embed <- embed.Build()

            awaiti <| e.Channel.SendMessageAsync b

        match command with
        | EditAction(act, mailId) ->
            EditAction.handle postmanType send displayMail e.Author.Id mailId act state

        | MainAction(act) ->
            let displayMails (mails: Mails.MailDb) =
                let getMails next =
                    match Mails.MailDb.tryFindByUserId e.Author.Id mails with
                    | Some mails ->
                        next mails
                    | None ->
                        match postmanType with
                        | SantaClaus ->
                            "Список писем пуст."
                        | Valentine ->
                            "Список валентинок пуст."
                        |> send

                getMails <| fun mails ->

                [
                    "№ | кому | описание"
                    yield! mails
                    |> List.mapi (fun i mailId ->
                        match Mails.MailDb.tryFindById mailId state.Mails with
                        | Some mail ->
                            let description =
                                let maxLength = 64
                                let description =
                                    mail.Data.Description
                                    |> String.replace "\n" " "
                                if description.Length < maxLength then
                                    description
                                else
                                    description.[0..maxLength - 1]

                            sprintf "%d | <@%d> | %s" i mail.Data.Recipient description
                        | None ->
                            sprintf "%d %s" i mailId
                    )
                ]
                |> String.concat "\n"
                |> send

            let getMailByIndex (mailIndex: MailIndex) next =
                match Mails.MailDb.tryFindByMailIndex e.Author.Id mailIndex state.Mails with
                | Some mail ->
                    next mail
                | None ->
                    let displayMailCommandName = MainAction.CommandNames.displayMails postmanType

                    match postmanType with
                    | SantaClaus ->
                        sprintf "Письмо под номером %d не найдено. Попробуйте еще раз вызвать список писем командой `%s` и указать номер нужного письма для редактирования."
                            mailIndex
                            displayMailCommandName
                    | Valentine ->
                        sprintf "Валентинка под номером %d не найдено. Попробуйте еще раз вызвать список писем командой `%s` и указать номер нужного письма для редактирования."
                            mailIndex
                            displayMailCommandName
                    |> send

                    state

            match act with
            | EditMail mailIndex ->
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

                EditAction.help postmanType
                |> send

                displayMail mail

                { state with
                    UserEditStates = userEditStates
                }

            | RemoveMail mailIndex ->
                getMailByIndex mailIndex <| fun mail ->

                match Mails.MailDb.removeByMail mail state.Mails with
                | Some mails ->
                    match postmanType with
                    | SantaClaus ->
                        sprintf "Поздравление № %d удалено." mailIndex
                    | Valentine ->
                        sprintf "Валентинка № %d удалена." mailIndex
                    |> send

                    displayMails mails

                    { state with
                        Mails = mails
                    }
                | None ->
                    match postmanType with
                    | SantaClaus ->
                        sprintf "Поздравление № %d не удалось удалить. Повторите еще раз." mailIndex
                    | Valentine ->
                        sprintf "Валентинку № %d не удалось удалить. Повторите еще раз." mailIndex
                    |> send

                    displayMails state.Mails

                    state

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

                EditAction.help postmanType
                |> send

                displayMail mail

                { state with
                    Mails = mails
                    UserEditStates = userEditStates
                }

            | DisplayMails ->
                displayMails state.Mails

                state

let create (postmanType: PostmanType) db =
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
                            reduce postmanType msg state
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
