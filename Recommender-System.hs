import DataFile

--Shabrawy's work

getFreqInList :: String -> [String] -> Int
getFreqInList _ []=0
getFreqInList user (x:xs)
						|user==x =1+getFreqInList user xs
						|otherwise =getFreqInList user xs

removeDoublicates ::Eq a=> [a] -> [a]
removeDoublicates []=[]
removeDoublicates (x:xs) 
						|presentIn x xs ==True =removeDoublicates xs
						|otherwise =x:removeDoublicates xs
						
presentIn :: Eq a=> a -> [a] -> Bool
presentIn _ [] =False
presentIn a (x:xs)
						|a==x =True
						|otherwise =presentIn a xs
						

createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList []=[]
createEmptyFreqList (x:xs)=(x,[]):createEmptyFreqList xs


getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]
getAllUsersStats []=[]
getAllUsersStats ((name,l):xs)=(name,createBuyWith (createEmptyFreqList items ) l):getAllUsersStats xs

createBuyWith :: [(String,[b])] -> [[String]] -> [(String, [(String ,Int)])]
createBuyWith [] _ =[]
createBuyWith ((name,list):xs) h = (name,(removeDoublicates(formTuble name (formList name h) (formList name h)))):(createBuyWith xs h)

formList :: String -> [[String]] -> [String]
formList _ []=[]
formList name (x:xs)
						|presentIn name x ==True =x++formList name xs
						|otherwise =formList name xs
						
formTuble :: String -> [String] -> [String] -> [(String,Int)]
formTuble _ [] _ = []
formTuble name (x:xs) list 
						|name==x =formTuble name xs list
						|otherwise =(x,getFreqInList x list):(formTuble name xs list)


purchasesIntersection :: Eq a => [(a,[(a,Int)])] -> [(a,[(a,[(a,Int)])])] -> [[(a,[(a,Int)])]]
purchasesIntersection _ []=[] 
purchasesIntersection a ((userName,list):xs)=(getIntersection a list):(purchasesIntersection a xs)

getIntersection :: Eq a => [(a,[(a,Int)])] -> [(a,[(a,Int)])] -> [(a,[(a,Int)])]
getIntersection _ []=[]
getIntersection a ((itemName,list):xs)
						|list==[] =getIntersection a xs
						|((getIntersectionHelper a (itemName,list))==(itemName,list)&&list/=[]) =getIntersection a xs
						|otherwise =(getIntersectionHelper a (itemName,list)):(getIntersection a xs)
						
getIntersectionHelper :: Eq a => [(a,[(a,Int)])] -> (a,[(a,Int)]) -> (a,[(a,Int)])
getIntersectionHelper [] a =a
getIntersectionHelper ((itemName,list):xs) (name,list2)
						|(itemName==name&&not (list==[])) =(name,(removeDoublicates (getUnionIntersection (list++list2) (list++list2))))
						|otherwise =getIntersectionHelper xs (name,list2)

getUnionIntersection :: Eq a => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
getUnionIntersection [] _ =[]
getUnionIntersection ((name,num):xs) list=(name,(getUnionIntersectionHelper name list)):(getUnionIntersection xs list)

getUnionIntersectionHelper :: Eq a=> a -> [(a,Int)] -> Int
getUnionIntersectionHelper _ [] =0
getUnionIntersectionHelper name ((vari,num):xs)
						|name==vari =num+(getUnionIntersectionHelper name xs)
						|otherwise =getUnionIntersectionHelper name xs
						
--end Shabrawy's work

						
-----------abdallah_elking's work ,
freqListUsers:: String -> [(String, Int)]
freqListUsers x=helpFreqList (dataOfX x (getAllUsersStats purchasesHistory) ) (listRemovedX x (getAllUsersStats purchasesHistory) )

listRemovedX:: String->[(String, [(String, [(String, Int)])])] -> [(String, [(String, [(String, Int)])])]
listRemovedX x ((y,z):ls)=if x==y then ls else (y,z):listRemovedX x ls 

dataOfX :: String->[(String,[(String,[(String,Int)])])]->[(String,[(String,Int)])]
dataOfX x ((y,z):ls)=if x==y then z else dataOfX x ls

helpFreqList :: [(String,[(String,Int)])] -> [(String, [(String, [(String, Int)])])] ->[(String, Int)] 
helpFreqList x y =getListOfFreq (purchasesIntersection x y) items


getListOfFreq :: Eq a=>[[(a,[(a,Int)])]] -> [a] ->[(a, Int)] 
getListOfFreq l []=[]
getListOfFreq l (x:xs)=if getOcc x l>0 then (x,getOcc x l):getListOfFreq l xs else getListOfFreq l xs

getOcc :: Eq a=> a ->[ [(a,[(a,Int)])] ] -> Int
getOcc x []=0
getOcc x (y:ys)=(getOcc2 x y) + (getOcc x ys)

getOcc2 :: Eq a=>a ->[(a,[(a,Int)])] -> Int
getOcc2 _ []=0
getOcc2 x ((_,y):ys)=(getOcc3 x y)+(getOcc2 x ys)

getOcc3 ::Eq a=> a ->[(a,Int)] -> Int
getOcc3 _ []=0
getOcc3 x ((y1,y2):ys)=if x==y1 then y2+getOcc3 x ys else getOcc3 x ys


recommendBasedOnUsers :: String -> String
recommendBasedOnUsers x= recB (freqListUsers x) (length2 (flattenFreq (freqListUsers x))) --question , not sure of logic

recB :: [(String, Int)]->Int->String  
recB x 0=""
recB x n= getElementAt (randomZeroToX (n-1)) (flattenFreq x)
 
getElementAt :: Int->[a]->a
getElementAt x (y:ys) | x==0 =y
                      | otherwise= getElementAt (x-1) ys

length2 :: [a]->Int
length2 []=0
length2 (x:xs)=1+(length2 xs)

flattenFreq :: [(a,Int)]->[a]		
flattenFreq []=[]
flattenFreq ((x,y):xs)= (dupsOfX x y) ++ (flattenFreq xs )

dupsOfX :: a->Int->[a]
dupsOfX _ 0=[]
dupsOfX x y = x:(dupsOfX x (y-1)) 


freqListItems:: String -> [(String, Int)]
freqListItems x= helpFreqListItems (dataOfX x (getAllUsersStats purchasesHistory))

helpFreqListItems :: [(String,[(String,Int)])]->[(String, Int)]
helpFreqListItems []=[]
helpFreqListItems ((x,y):xs)=if length2(flattenFreq y)>0 then (x,length2(flattenFreq y)):(helpFreqListItems xs) else helpFreqListItems xs

--------end of abdallah_elking's work

------ Here starts Eslam
freqListCart:: String ->[String] -> [(String, Int)]
freqListCart user [] = []
freqListCart user cart = getAllPairsInAlist user desired cart where desired =getDesiredItems user cart


dataOfX2 :: String->[(String,[(String,Int)])]->[(String,Int)]
dataOfX2 x2 ((y,z):ls)=if x2==y then z else dataOfX2 x2 ls

getDesiredItems user cart = removeDoublicates (getDesiredItemsDup user cart)

getDesiredItemsDup :: String -> [String] -> [String]
getDesiredItemsDup _ [] = []
getDesiredItemsDup user (x:xs) = getDIH(dataOfX2 x userList)++(getDesiredItemsDup user xs)  where userList=dataOfX user (getAllUsersStats purchasesHistory) 
-- x:xs is the cart 
getDIH:: [(String,Int)] -> [String]
getDIH [] = []
getDIH ((item,f):xs) = [item]++getDIH xs

getFreq :: String -> [(String,Int)] -> Int
getFreq _ [] = 0
getFreq item ((str,int):t) = if item==str then int+getFreq item t else getFreq item t

getOneItemTotal:: String -> String -> [String] -> Int
getOneItemTotal _ _ [] = 0
getOneItemTotal user item (x:xs) = getFreq item (dataOfX2 x userList)+getOneItemTotal user item xs where userList = dataOfX user (getAllUsersStats purchasesHistory)

getThePair :: String -> String ->[String] -> (String,Int)

getThePair user item cart = (item,tot) where tot = getOneItemTotal user item cart
getAllPairsInAlist _ [] _ = [] 
getAllPairsInAlist user (item:items) cart = [getThePair user item cart]++getAllPairsInAlist user items cart

getAllStrings [] = []
getAllStrings ((str1,i1):tail) = [str1]++getAllStrings(tail)
getAllItems l1 l2 = (getAllStrings l1) ++ (getAllStrings l2)


freqListCartAndItems:: String -> [String] -> [(String, Int)]
freqListCartAndItems user cart = flciH rD flc fli
	where 
	rD = removeDoublicates (getAllItems flc fli)
	flc = freqListCart user cart 
	fli = freqListItems user
flciH [] _ _ = []
flciH (x:xs) flc fli = [(x,(getFreq x flc)+(getFreq x fli))]++(flciH xs flc fli)



------ Eslam Finished

----- Arousiii start

recommend :: [Char] -> [[Char]] -> [Char]
recommend user [] = getFromItems items (randomZeroToX (length33 items)) 0
--recommend user []    = getElementAt (randomZeroToX (length33 items)) items 
recommend user cart =  getElementAt (randomZeroToX 1) [a,b]
		   where
		   a = recommendBasedOnItemsInCart user cart
		   b = recommendBasedOnUsers user

recommendBasedOnItemsInCart:: [Char] -> [[Char]] -> [Char]
recommendBasedOnItemsInCart x [] = recB (freqListCartAndItems x []) (length2 (flattenFreq (freqListCartAndItems x []))) 
recommendBasedOnItemsInCart x y = recB (freqListCartAndItems x y) (length2 (flattenFreq (freqListCartAndItems x y))) 

--recommendEmptyCart:: [Char] -> [Char]
--recommendEmptyCart x = getInfoo putAlone (freqListItems x) (randomZeroToX (lengthOfTheUser x)) 0

recommendEmptyCart:: [Char] -> [Char]
recommendEmptyCart x = getElementAt (randomZeroToX ((length2 flatFreqListItems)-1)) flatFreqListItems where flatFreqListItems = flattenFreq (freqListItems x)


lengthOfTheUser:: [Char] -> Int
lengthOfTheUser x  = length(freqListItems x) - 1

length33 x = length x -1
putAlone:: [(a,b)] -> [a]
putAlone [] = []
putAlone ((userNum,_):xs) =  userNum:putAlone xs

getInfoo:: Num a => b -> [([Char],c)] -> a -> a -> String
getInfoo putAlone [] _ _ = ""
getInfoo putAlone ((userNum,k):xs) q z  = if z == q then userNum
			else getInfoo putAlone (xs) q (increment1 z)

getFromItems (x:xs) q z = if z == q then x 
			else getFromItems xs q (increment1 z)

increment1:: Num a => a -> a
increment1 x = x + 1

------ Arousii endd
--applyn :: Num a => a -> 
applyn1p 0 _ _ = []
applyn1p n f x= [f x]++(applyn1p (n-1) f x)

applyn2p 0 _ _ _ = []
applyn2p n f x y= [f x y]++(applyn2p (n-1) f x y)

-- countOcc :: Num b => [a] -> [(a,b)]
-- countOcc [] = []
-- countOcc [a] = [(a,1)]
-- countOcc

-- countOccOfOne b [] = 0
-- countOccOfOne b (x:xs) = if b==x then 1+countOccOfOne b xs  else countOccOfOne b xs