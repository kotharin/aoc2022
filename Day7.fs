namespace Day7

module Shared = 

    open System.IO

    type FileData = {
        Name: string
        Size: int
    }

    type Action =
        | List
        | ChangeDirectory of string
        | Dir of string
        | FileInfo of FileData

    with
        static member parse (s:string) =

            let parts = s.Split(' ')
            if (parts.[0] = "$") then
                // command
                if (parts.[1] = "ls") then
                    List
                else
                    // cd
                    ChangeDirectory(parts.[2])
            else
                // File or Dir
                if (parts.[0] = "dir") then
                    Dir(parts.[1])
                else
                    FileInfo({FileData.Name=parts.[1]; FileData.Size = (int)parts.[0]})

    type Tree = {
        Name: string
        Dirs: Tree List
        Files: FileData List
        Parent: string option
    } with
        static member addDir dir tree =
            let newDir = {
                Name = tree.Name + "-" + dir
                Dirs = List.empty
                Files = List.empty
                Parent = Some tree.Name
            }
            {tree with Dirs = newDir::tree.Dirs}
        static member addFile (file:FileData) tree =
            {tree with Files = file::tree.Files}

    let directorySize dir allDirectories= 
        let rec dirSize dir ad size =
            // Get the directory tree from the map
            let tree = Map.find dir ad
            // Recurse the subtrees
            let subTreeSize = 
                tree.Dirs
                |> List.fold (fun state st ->
                    let sz = dirSize st.Name ad size
                    state + sz
                ) size

            // Get the file sizes
            let fileSizes =
                tree.Files
                |> List.sumBy (fun fd -> fd.Size)

            size + subTreeSize + fileSizes
        dirSize dir allDirectories 0

    let extractDirectoryInfo lines =
        
        let root = {
            Name = "-/"
            Dirs = List.empty
            Files = List.empty
            Parent = Some ""
        }

        let tree = {
            Name = ""
            Dirs = [root]
            Files = List.empty
            Parent = None
        }

        lines
        |> Array.fold (fun (ct,allDirs) line ->
            
            match Action.parse line with
            | List  ->
                ct, allDirs
            | ChangeDirectory dir ->
                let dirName = (ct.Name + "-" + dir)
                //printfn "dir:%s" dirName
                //printfn "pd:%A" ct.Dirs
                // Get the current dir tree
                // if parent (..), get that from dictionary
                let newCT =
                    if (dir = "..") then
                        Map.find ct.Parent.Value allDirs
                    else
                        List.find (fun t -> t.Name = dirName) ct.Dirs
                newCT, allDirs
            | Dir d -> 
                // add to list of dirs
                let newCT = Tree.addDir d ct
                newCT, (Map.add ct.Name newCT allDirs)
            | FileInfo fd -> 
                let newCT = Tree.addFile fd ct
                newCT, (Map.add ct.Name newCT allDirs) 
        ) (tree, Map.empty)
        |> snd
        //printfn "AD:%A" allDirectories
        // Get sizes and filter and sum
        
module Part1 = 

    open System.IO
    open Shared

    let solution inputFile = 
        let lines = File.ReadAllLines inputFile

        let allDirectories = extractDirectoryInfo lines

        allDirectories.Keys
        |> Seq.map (fun dir ->
            dir, (directorySize dir allDirectories)
        )
        |> Seq.filter (fun (dir, size) -> 
            size <= 100000
        )
        |> Seq.sumBy (fun (d,s) -> s) 

module Part2 = 

    open System.IO
    open Shared

    let solution inputFile =

        let TOTAL_DISK_SPACE = 70000000
        let REQUIRED_SPACE = 30000000

        let lines = File.ReadAllLines inputFile

        let allDirectories = extractDirectoryInfo lines

        let directorySizes, listOfSizes =
            allDirectories.Keys
            |> Seq.fold (fun state dir ->
                let map, list = state
                let size = (directorySize dir allDirectories)
                Map.add dir size map, (dir, size)::list
            ) (Map.empty, List.empty)

        let rootSize = Map.find "-/" directorySizes

        let unusedSpace =  TOTAL_DISK_SPACE - rootSize

        let spaceNeeded = REQUIRED_SPACE - unusedSpace

        listOfSizes
        |> List.filter (fun (name, size) -> size >= spaceNeeded)
        |> List.minBy (fun (name, size) -> size)
        |> snd