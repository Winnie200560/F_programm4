// Лабораторная №4
// Задание 1
// Дерево содержит строки. В конец каждой строки приписать ее же.

open System

type 't BinaryTree = 
    Node of 't * 't BinaryTree * 't BinaryTree
    | Nil

let rnd = Random()

// Генерация случайной строки
let randomString () =
    let length = rnd.Next(3, 6)

    let rec build n =
        if n = 0 then ""
        else
            let c = char (rnd.Next(97, 123))
            string c + build (n - 1)

    build length

// Функция вставки
let rec insert t x = 
    match t with
    | Nil -> Node (x, Nil, Nil)
    | Node(z,L,R) ->
        if x < z then
            Node(z, insert L x, R)
        else 
            Node(z, L, insert R x)

// map для дерева
let rec mapTree f tree =
    match tree with
    | Nil -> Nil
    | Node(data,left,right) ->
        Node(
            f data,
            mapTree f left,
            mapTree f right
        )

// Удвоение строки
let duplicateString s = 
    s + s

// Вывод дерева
let spaces n = List.fold (fun s _ -> s+ "    ") "" [0..n]

let printTree tree =
    let rec print t level =
        match t with
        | Node(data, left, right) ->
            print left (level + 1)
            printfn "%s%s" (spaces level) data
            print right (level + 1)
        | Nil -> ()
    print tree 0

[<EntryPoint>]
let main args =

    printf "Количество элементов: "
    let n = int(Console.ReadLine())

    // Создание списка
    let A =
        [ 
        for i in 1..n -> 
            randomString() 
        ]

    printfn "Исходный список: %A" A

    // Построение дерева
    let tree = List.fold insert Nil A

    printfn "\nИсходное дерево:"
    printTree tree

    let newTree = mapTree duplicateString tree

    printfn "\nНовое дерево:"
    printTree newTree

    0
