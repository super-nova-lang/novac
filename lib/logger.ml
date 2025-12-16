open Easy_logging

let log = Logging.make_logger "Novac" Debug [ Cli Debug ]
let lex = Logging.get_logger "Novac.Lexer"
let par = Logging.get_logger "Novac.Parser"
