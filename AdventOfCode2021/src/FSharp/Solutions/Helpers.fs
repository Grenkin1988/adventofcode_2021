module Solutions.Helpers

let readInput fileName = 
    let fileName = sprintf "%s/Input/%s" __SOURCE_DIRECTORY__ fileName
    Helpers.readLines fileName
