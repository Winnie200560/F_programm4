// Лабораторная №4
// Задача 2
// Выяснить, содержится ли в дереве элемент с заданным значением.

open System

type BinaryTree =
    | Node of int * BinaryTree * BinaryTree
    | Empty

let rnd = Random()

// Создание дерева со случайными числами
let rec createTree depth =
    if depth = 0 then
        Empty
    else
        Node(
            rnd.Next(1,100),  // От 1 до 99
            createTree (depth - 1),
            createTree (depth - 1)
        )

// Симметричный обход дерева 
let rec printTree tree =
    match tree with
    | Node(data,left,right) ->
        printTree left
        printf "%d " data
        printTree right
    | Empty -> ()

// fold для дерева
let rec foldTree f acc tree =
    match tree with
    | Empty -> acc
    | Node(data,left,right) ->
        let processLeft = foldTree f acc left
        let current = f processLeft data
        foldTree f current right

// Проверка наличия элемента
let includeValue value tree =
    foldTree (fun found x -> found || (x = value)) false tree

[<EntryPoint>]
let main args =

    printf "Введите глубину дерева: "
    let n = int(Console.ReadLine())
    
    if n < 0 then
        printfn "Ошибка! Глубина дерева должна быть положительна!"
    else 
        let tree = createTree n

        printfn "Дерево:"
        printTree tree

        printf "\nВведите число для поиска: "
        let value = int(Console.ReadLine())

        if includeValue value tree then
            printfn "Число найдено"
        else
            printfn "Число не найдено"

    0