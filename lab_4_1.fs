// Лабораторная №4
// Бинарные деревья заполняются случайными строками.
// Дерево содержит строки. В конец каждой строки приписать ее же

open System

type BinaryTree = 
    | Node of string * BinaryTree * BinaryTree
    | Empty

// Генератор случайных чисел
let rnd = Random()

// Генерация случайной строки
let randomString () =
    let letters = "abcde"
    let length = rnd.Next(3,6) // От 3 до 5 символов
    
    let rec buildString n = 
        if n = 0 then 
            "" // Возвращается пустая строка
        else 
            string letters.[rnd.Next(letters.Length)] + buildString (n - 1)

    buildString length

// Создание случайного дерева
let rec createTree depth =
    if depth = 0 then
        Empty
    else
        Node(
            randomString(),
            createTree (depth - 1),
            createTree (depth - 1)
        )

// map для дерева
let rec mapTree f tree =
    match tree with
    | Empty -> Empty
    | Node(data,left,right) ->
        Node(
            f data,
            mapTree f left,
            mapTree f right
        )

// Функция удвоения строки
let duplicateString s =
    s + s

// Симметричный обход дерева 
let rec printTree tree = 
    match tree with 
    | Node (data, left, right) ->
        printTree left
        printfn "%s" data
        printTree right
    | Empty -> ()


[<EntryPoint>]
let main args =

    printf "Введите глубину дерева: "
    let n = int(Console.ReadLine())
    
    if n < 0 then
        printfn "Ошибка! Глубина дерева должна быть положительна!"
    else 
        let tree = createTree n

        printfn "Исходное дерево:"
        printTree tree

        let newTree = mapTree duplicateString tree

        printfn "\nНовое дерево:"
        printTree newTree

    0