import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import Common

main :: IO()
main = do
	contents <- TextIO.readFile "input.txt"
	let rows = Text.lines contents
	let vals = parseArrs rows
	let totalDiff = getTotalDiff vals
	print totalDiff
	
