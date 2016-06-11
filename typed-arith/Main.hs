import System.Enviornment

main :: IO ()
main = getArgs >>= print . helloifier . head

helloifier s = "hi, " ++ s
