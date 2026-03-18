// Задание 2
// Выяснить, содержится ли в дереве элемент с заданным значением

open System

type 't BinaryTree = 
    Node of 't * 't BinaryTree * 't BinaryTree
    | Nil

let rnd = Random()

// Вставка в дерево
let rec insert t x = 
    match t with
    | Nil -> Node (x, Nil, Nil)
    | Node(z,L,R) ->
        if x < z then
            Node(z, insert L x, R)
        else 
            Node(z, L, insert R x)

// Вывод дерева
let spaces n = List.fold (fun s _ -> s+ "    ") "" [0..n]

let printTree tree =
    let rec print t level =
        match t with
        | Node(data, left, right) ->
            print left (level + 1)
            printfn "%s%d" (spaces level) data
            print right (level + 1)
        | Nil -> ()
    print tree 0

// fold для дерева
let rec foldTree f acc tree =
    match tree with
    | Nil -> acc
    | Node(data,left,right) ->
        let leftRes = foldTree f acc left
        let cur = f leftRes data
        foldTree f cur right

// Проверка наличия элемента
let includeValue value tree =
    foldTree (fun found x -> found || (x = value)) false tree

[<EntryPoint>]
let main args =

    printf "Количество элементов: "
    let n = int(Console.ReadLine())

    // Генерация списка
    let A =
        [ 
        for i in 1..n -> 
            rnd.Next(1, 101) 
        ]

    printfn "Список: %A" A

    // Построение дерева
    let tree = List.fold insert Nil A 

    printfn "\nДерево:"
    printTree tree

    printf "\nВведите число для поиска: "
    let value = int(Console.ReadLine())

    if includeValue value tree then
        printfn "Число найдено"
    else
        printfn "Число не найдено"

    0
