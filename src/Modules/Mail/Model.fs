module Mail.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types
open Db

type MailId = string
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MailId =
    let generateNew (): MailId =
        ObjectId.GenerateNewId().ToString()

type MailIndex = int

module Mails =
    type MailData =
        {
            Author: UserId
            Recipient: UserId
            IsDelivered: bool
            Description: string
            From: string
            ImageUrl: string
        }
        static member Empty =
            {
                Author = 0UL
                Recipient = 0UL
                IsDelivered = false
                Description = "(пусто)"
                From = "Аноним"
                ImageUrl = ""
            }
        static member Serialize (data: MailData) =
            data |> Json.ser
        static member Deserialize json =
            try
                Ok (Json.des json)
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type Mail = CommonDb.Data<MailId, Version, MailData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Mail =
        let create (id: MailId) (data: MailData): Mail =
            CommonDb.Data.create id Version.V0 data

    type MailDb =
        {
            Db: CommonDb.GuildData<MailId, Version, MailData>
            UserMails: Map<UserId, MailId list>
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module MailDb =
        let createData (id: MailId) =
            Mail.create id MailData.Empty

        let init collectionName (db: IMongoDatabase): MailDb =
            let db =
                CommonDb.GuildData.init
                    createData
                    (fun ver x ->
                        match Option.get ver with
                        | Version.V0 ->
                            None, Serialization.BsonSerializer.Deserialize<Mail>(x)
                        | x ->
                            failwithf "Version = %A not implemented" x
                    )
                    collectionName
                    db
            {
                Db = db
                UserMails =
                    db.Cache
                    |> Map.fold
                        (fun st mailId mail ->
                            st
                            |> Map.addOrModWith
                                mail.Data.Author
                                (fun () -> [mailId])
                                (fun mailIds -> mailId::mailIds)
                        )
                        Map.empty
            }

        let set mailId setAdditionParams (guildData: MailDb) =
            let db =
                CommonDb.GuildData.set
                    createData
                    mailId
                    setAdditionParams
                    guildData.Db
            {
                Db = db
                UserMails =
                    // todo: optimize
                    match CommonDb.GuildData.tryFind mailId db with
                    | Some x ->
                        guildData.UserMails
                        |> Map.addOrModWith
                            x.Data.Author
                            (fun () -> [mailId])
                            (fun mailIds ->
                                mailId::mailIds
                                |> List.distinct
                            )
                    | None ->
                        failwithf "not found %s mailId" mailId
            }

        let drop (db: IMongoDatabase) (items: MailDb): MailDb =
            {
                Db = CommonDb.GuildData.drop db items.Db
                UserMails = Map.empty
            }

        let tryFindById mailId (items: MailDb): Mail option =
            CommonDb.GuildData.tryFind mailId items.Db

        let tryFindByUserId (userId: UserId) (db: MailDb): Option<MailId list> =
            Map.tryFind userId db.UserMails

        let tryFindByMailIndex (userId: UserId) (mailIndex: MailIndex) (db: MailDb): Mail option =
            Map.tryFind userId db.UserMails
            |> Option.bind (fun mails ->
                if 0 <= mailIndex && mailIndex < mails.Length then
                    let mailId = mails.[mailIndex]
                    tryFindById mailId db
                else
                    None
            )

module UserEditStates =
    type MainData =
        {
            Editing: MailId option
        }
        static member Empty =
            {
                Editing = None
            }
        static member Serialize (data: MainData) =
            data |> Json.ser
        static member Deserialize json =
            try
                let res: MainData = Json.des json
                Ok res
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type Id = UserId

    type Data = CommonDb.Data<Id, Version, MainData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Data =
        let create id data =
            CommonDb.Data.create id Version.V0 data

    type GuildData = CommonDb.GuildData<Id, Version, MainData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildData =
        let createData id =
            Data.create id MainData.Empty

        let init collectionName (db: IMongoDatabase): GuildData =
            CommonDb.GuildData.init
                createData
                (fun ver x ->
                    match Option.get ver with
                    | Version.V0 ->
                        None, Serialization.BsonSerializer.Deserialize<Data>(x)
                    | x ->
                        failwithf "Version = %A not implemented" x
                )
                collectionName
                db

        let set id setAdditionParams (guildData: GuildData) =
            CommonDb.GuildData.set
                createData
                id
                setAdditionParams
                guildData

        let drop (db: IMongoDatabase) (items: GuildData) =
            CommonDb.GuildData.drop db items

        let tryFindById id (items: GuildData): Data option =
            CommonDb.GuildData.tryFind id items
