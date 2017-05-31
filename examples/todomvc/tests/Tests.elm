module Tests
    exposing
        ( noOpDoesNothing
        , updateFieldSetsField
        , editingEntrySetsEditingField
        , updateEntrySetsDescriptionField
        , addIncrementsUid
        , addDoesntAddIfEmptyField
        , addAddsIfNonemptyField
        , deleteDeletesEntryWithId
        , deleteCompleteDeletesCompletedEntries
        , checkSetsCompletedFlag
        , checkAllSetsCompletedFlagEverywhere
        , changeVisibilitySetsVisibilityField
        )

import ArchitectureTest
import ArchitectureTest.Types exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import TodoMVC exposing (Model, Msg(..), emptyModel, updateWithStorage)
import String


noOp : Fuzzer Msg
noOp =
    Fuzz.constant NoOp


updateField : Fuzzer Msg
updateField =
    Fuzz.string
        |> Fuzz.map UpdateField


editingEntry : Fuzzer Msg
editingEntry =
    Fuzz.map2 EditingEntry
        Fuzz.int
        Fuzz.bool


updateEntry : Fuzzer Msg
updateEntry =
    Fuzz.map2 UpdateEntry
        Fuzz.int
        Fuzz.string


add : Fuzzer Msg
add =
    Fuzz.constant Add


delete : Fuzzer Msg
delete =
    Fuzz.int
        |> Fuzz.map Delete


deleteComplete : Fuzzer Msg
deleteComplete =
    Fuzz.constant DeleteComplete


check : Fuzzer Msg
check =
    Fuzz.map2 Check
        Fuzz.int
        Fuzz.bool


checkAll : Fuzzer Msg
checkAll =
    Fuzz.bool
        |> Fuzz.map CheckAll


changeVisibility : Fuzzer Msg
changeVisibility =
    Fuzz.string
        |> Fuzz.map ChangeVisibility


msg : Fuzzer Msg
msg =
    Fuzz.oneOf
        [ noOp
        , updateField
        , editingEntry
        , updateEntry
        , add
        , delete
        , deleteComplete
        , check
        , checkAll
        , changeVisibility
        ]


app : TestedApp Model Msg
app =
    { model = ConstantModel emptyModel
    , update = NormalUpdate updateWithStorage
    , msgFuzzer = msg
    }



-- Tests


noOpDoesNothing : Test
noOpDoesNothing =
    ArchitectureTest.msgTest
        "Msg NoOp does nothing"
        app
        noOp
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel
                |> Expect.equal modelBeforeMsg


updateFieldSetsField : Test
updateFieldSetsField =
    ArchitectureTest.msgTest
        "Msg UpdateField sets the 'field' in Model"
        app
        updateField
    <|
        \_ _ _ msg finalModel ->
            case msg of
                UpdateField field ->
                    finalModel.field
                        |> Expect.equal field

                _ ->
                    Expect.pass


editingEntrySetsEditingField : Test
editingEntrySetsEditingField =
    ArchitectureTest.msgTest
        "Msg EditingEntry changes the 'editing' field of an Entry"
        app
        editingEntry
    <|
        \_ _ _ msg finalModel ->
            case msg of
                EditingEntry id isEditing ->
                    finalModel.entries
                        |> List.filter (\entry -> entry.id == id)
                        |> List.all (\entry -> entry.editing == isEditing)
                        |> Expect.true
                            ("Entry with ID "
                                ++ toString id
                                ++ " should have had editing = "
                                ++ toString isEditing
                            )

                _ ->
                    Expect.pass


updateEntrySetsDescriptionField : Test
updateEntrySetsDescriptionField =
    ArchitectureTest.msgTest
        "Msg UpdateEntry changes the 'description' field of an Entry"
        app
        updateEntry
    <|
        \_ _ _ msg finalModel ->
            case msg of
                UpdateEntry id description ->
                    finalModel.entries
                        |> List.filter (\entry -> entry.id == id)
                        |> List.all (\entry -> entry.description == description)
                        |> Expect.true
                            ("Entry with ID "
                                ++ toString id
                                ++ " should have had description = \""
                                ++ description
                                ++ "\""
                            )

                _ ->
                    Expect.pass


addIncrementsUid : Test
addIncrementsUid =
    ArchitectureTest.msgTest
        "Msg Add increments UID (even if it doesn't add an entry)"
        app
        add
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.uid
                |> Expect.equal (modelBeforeMsg.uid + 1)


addDoesntAddIfEmptyField : Test
addDoesntAddIfEmptyField =
    ArchitectureTest.msgTestWithPrecondition
        "Msg Add doesn't add an entry if empty model.field"
        app
        add
        (\model -> String.isEmpty model.field)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.entries
                |> Expect.equalLists modelBeforeMsg.entries


addAddsIfNonemptyField : Test
addAddsIfNonemptyField =
    ArchitectureTest.msgTestWithPrecondition
        "Msg Add adds an entry if nonempty model.field"
        app
        add
        (\model -> not <| String.isEmpty model.field)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.entries
                |> Expect.notEqual modelBeforeMsg.entries


deleteDeletesEntryWithId : Test
deleteDeletesEntryWithId =
    ArchitectureTest.msgTest
        "Msg Delete deletes an entry with given id"
        app
        delete
    <|
        \_ _ _ msg finalModel ->
            case msg of
                Delete id ->
                    finalModel.entries
                        |> List.filter (\entry -> entry.id == id)
                        |> List.length
                        |> Expect.equal 0

                _ ->
                    Expect.pass


deleteCompleteDeletesCompletedEntries : Test
deleteCompleteDeletesCompletedEntries =
    ArchitectureTest.msgTest
        "Msg DeleteComplete deletes entries with completed = True"
        app
        deleteComplete
    <|
        \_ _ _ _ finalModel ->
            finalModel.entries
                |> List.filter (\entry -> entry.completed)
                |> List.length
                |> Expect.equal 0


checkSetsCompletedFlag : Test
checkSetsCompletedFlag =
    ArchitectureTest.msgTest
        "Msg Check sets 'completed' flag of entry with given id"
        app
        check
    <|
        \_ _ _ msg finalModel ->
            case msg of
                Check id isCompleted ->
                    finalModel.entries
                        |> List.filter (\entry -> entry.id == id)
                        |> List.all (\entry -> entry.completed == isCompleted)
                        |> Expect.true
                            ("Entry with ID "
                                ++ toString id
                                ++ " should have had completed = "
                                ++ toString isCompleted
                            )

                _ ->
                    Expect.pass


checkAllSetsCompletedFlagEverywhere : Test
checkAllSetsCompletedFlagEverywhere =
    ArchitectureTest.msgTest
        "Msg CheckAll sets 'completed' flag for all entries"
        app
        checkAll
    <|
        \_ _ _ msg finalModel ->
            case msg of
                CheckAll isCompleted ->
                    finalModel.entries
                        |> List.all (\entry -> entry.completed == isCompleted)
                        |> Expect.true
                            ("All entries should have had completed = "
                                ++ toString isCompleted
                            )

                _ ->
                    Expect.pass


changeVisibilitySetsVisibilityField : Test
changeVisibilitySetsVisibilityField =
    ArchitectureTest.msgTest
        "Msg ChangeVisibility sets the 'visibility' field of the Model"
        app
        changeVisibility
    <|
        \_ _ _ msg finalModel ->
            case msg of
                ChangeVisibility visibility ->
                    finalModel.visibility
                        |> Expect.equal visibility

                _ ->
                    Expect.pass
