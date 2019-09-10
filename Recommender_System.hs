import System.Random
import System.IO.Unsafe

randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))


occursIn :: Eq a => a -> [a] -> Bool
occursIn x [] = False
occursIn a (x:xs) = if a == x then True
					else occursIn a xs

occurs :: Eq a => a -> [[a]] -> [[a]]
occurs a [] = []
occurs a (x:xs) = if occursIn a x then x:occurs a xs
					else occurs a xs

countOccur:: Eq a => a -> [a] -> Int
countOccur a [] = 0
countOccur a (x:xs) | (a == x) = 1 + (countOccur a xs)
					| otherwise = (countOccur a xs)

remove:: Eq a => a -> [a] -> [a]
remove a [] = []
remove a (x:xs) | (a == x) = remove a xs
				| otherwise = [x] ++ (remove a xs)

users = ["user1", "user2", "user3", "user4"]
items = ["item1", "item2", "item3", "item4", "item5", "item6"]
purchasesHistory = [("user1", [["item1", "item2", "item3"], ["item1", "item2", "item4"]]),
					("user2", [["item2", "item5"], ["item4", "item5"]]),
					("user3", [["item3", "item2"]]),
					("user4", [])]




createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList [] = []
createEmptyFreqList (item:otherItems) = (item,[]):(createEmptyFreqList otherItems)



 
getAllUsersStats :: [(String,[[String]])] -> [(String,[(String,[(String,Int)])])]
getAllUsersStats [] = []
getAllUsersStats purchases = map getUserStats purchases

getUserStats:: (String, [[String]]) -> (String, [(String, [(String, Int)])])
getUserStats (user, carts) = (user, getIntersections items carts)

getIntersections:: [String] -> [[String]] -> [(String, [(String, Int)])]
getIntersections [] carts = []
getIntersections (item:otherItems) carts = (item, getIntersections2 item items (concat (occurs item carts) ) ):(getIntersections otherItems carts)

getIntersections2:: String -> [String] -> [String] -> [(String, Int)]
getIntersections2 item [] cartList = []
getIntersections2 item (itemElement:otherItems) cartList 
		| (item == itemElement) = getIntersections2 item otherItems cartList
		| (occursIn itemElement cartList) = (itemElement, (countOccur itemElement cartList) ):(getIntersections2 item otherItems (remove itemElement cartList) )
		| otherwise = getIntersections2 item otherItems cartList




--purchasesIntersection::Eq a => [(a,[(a,Int)])] -> [(a,[(a,[(a,Int)])])] -> [[(a,[(a,Int)])]]
purchasesIntersection :: [(a,[(String,Int)])] -> [(b,[(c,[(String,Int)])])] -> [[(a,[(String,Int)])]]
purchasesIntersection userStats [] = []
purchasesIntersection userStats otherUsers = map (purchasesIntersection2 userStats) otherUsers

--purchasesIntersection2:: Eq a => [(a,[(a,Int)])] -> (a,[(a,[(a,Int)])]) -> [(a,[(a,Int)])]
purchasesIntersection2 :: [(a,[(String,Int)])] -> (b,[(c,[(String,Int)])]) -> [(a,[(String,Int)])]
purchasesIntersection2 userStats (user, otherUserStats) = getPurchasesIntersection userStats otherUserStats

--getPurchasesIntersection:: Eq a => [(a,[(a,Int)])] -> [(a,[(a,Int)])] -> [(a,[(a,Int)])]
getPurchasesIntersection :: [(a,[(String,Int)])] -> [(b,[(String,Int)])] -> [(a,[(String,Int)])]
getPurchasesIntersection [] [] = []
getPurchasesIntersection ((itemA, []):userStatsA) ((itemB, intersectsB):userStatsB) = getPurchasesIntersection userStatsA userStatsB
getPurchasesIntersection ((itemA, intersectsA):userStatsA) ((itemB, []):userStatsB) = getPurchasesIntersection userStatsA userStatsB
getPurchasesIntersection ((itemA, intersectsA):userStatsA) ((itemB, intersectsB):userStatsB) = (itemA, (getPurchasesIntersection2 intersectsA intersectsB) ):(getPurchasesIntersection userStatsA userStatsB)

--getPurchasesIntersection2:: Eq a => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
getPurchasesIntersection2 :: [(String,Int)] -> [(String,Int)] -> [(String,Int)]
getPurchasesIntersection2 a b = mergeFreqList items (a ++ b)

mergeFreqList:: Eq a => [a] -> [(a,Int)] -> [(a,Int)]
mergeFreqList [] freqList = []
mergeFreqList (item:otherItems) freqList	| (findFreq item freqList == 0) = mergeFreqList otherItems freqList
											| otherwise = (item, findFreq item freqList):(mergeFreqList otherItems freqList)

findFreq:: Eq a => a -> [(a,Int)] -> Int
findFreq a [] = 0
findFreq a ((x, freq):freqList) | (a == x) = freq + (findFreq a freqList)
								| otherwise = (findFreq a freqList)




freqListUsers:: String -> [(String, Int)]
freqListUsers user = mergeFreqList items (mergeFreqListMod (purchasesIntersection (extractUser user (getAllUsersStats purchasesHistory)) (removeUser user (getAllUsersStats purchasesHistory))))

mergeFreqListMod:: [[(a,[(String,Int)])]] -> [(String, Int)]
mergeFreqListMod [] = []
mergeFreqListMod (freqList:otherFreqLists) = (mergeFreqListModHelper freqList) ++ (mergeFreqListMod otherFreqLists)

mergeFreqListModHelper:: [(a,[(String,Int)])] -> [(String, Int)]
mergeFreqListModHelper [] = []
mergeFreqListModHelper ((item,freqList):otherItems) = freqList ++ mergeFreqListModHelper otherItems

extractUser:: String -> [(String,[(String,[(String,Int)])])] -> [(String,[(String,Int)])]
extractUser a ((b, i):xs)	| (a == b) = i
							| otherwise = extractUser a xs

removeUser:: String -> [(String,[(String,[(String,Int)])])] -> [(String,[(String,[(String,Int)])])]
removeUser a ((b, i):xs)	| (a == b) = xs
							| otherwise = [(b,i)] ++ (removeUser a xs)




recommendBasedOnUsers :: String -> String
recommendBasedOnUsers user	| (freqListUsers user == []) = ""
							| otherwise = (generateItemList (freqListUsers user))!!(randomZeroToX ( (totalItems (freqListUsers user)) - 1))

--recommendBasedOnUsers :: String -> String
--recommendBasedOnUsers user	= recommendBasedOnUsersHelper (freqListUsers user)
--recommendBasedOnUsersHelper:: [(String, Int)] -> String
--recommendBasedOnUsersHelper [] = ""
--recommendBasedOnUsersHelper freqList = (generateItemList freqList)!!(randomZeroToX ( (totalItems freqList) - 1))

totalItems:: [(String, Int)] -> Int
totalItems [] = 0
totalItems ((b, i):xs) = i + totalItems xs

generateItemList:: [(String, Int)] -> [String]
generateItemList [] = []
generateItemList ((item, 0):freqList) = generateItemList freqList
generateItemList ((item, n):freqList) = item:(generateItemList ((item, n-1):freqList))




freqListItems:: String -> [(String, Int)]
freqListItems user = mergeFreqList items (mergeFreqListModHelper (extractUser user (getAllUsersStats purchasesHistory)) )




freqListCart:: String ->[String] -> [(String, Int)] 
freqListCart user cart = mergeFreqList items (mergeFreqListMod2 cart (extractUser user (getAllUsersStats purchasesHistory)))

mergeFreqListMod2:: [String] -> [(String,[(String,Int)])] -> [(String, Int)]
mergeFreqListMod2 cart [] = []
mergeFreqListMod2 cart ((item,freqList):otherItems)	| (occursIn item cart) = freqList ++ mergeFreqListMod2 cart otherItems
													| otherwise = mergeFreqListMod2 cart otherItems




freqListCartAndItems:: String -> [String] -> [(String, Int)]
freqListCartAndItems user cart = mergeFreqList items ((freqListItems user) ++ (freqListCart user cart))




recommendEmptyCart :: String -> String
recommendEmptyCart user	| (freqListItems user == []) = ""
						| otherwise = (generateItemList (freqListItems user))!!(randomZeroToX ( (totalItems (freqListItems user)) - 1))




recommendBasedOnItemsInCart :: String -> [String] -> String
recommendBasedOnItemsInCart user cart	| (freqListItems user == []) = ""
										| otherwise = (generateItemList (freqListCartAndItems user cart))!!(randomZeroToX ( (totalItems (freqListCartAndItems user cart)) - 1))




recommend :: String -> [String] -> String
recommend user cart | (recommendBasedOnItemsInCart user cart == "") && (recommendBasedOnUsers user == "") = items!!(randomZeroToX ((length items) - 1))
					| (randomZeroToX 1 == 0) = recommendBasedOnItemsInCart user cart
					| otherwise = recommendBasedOnUsers user