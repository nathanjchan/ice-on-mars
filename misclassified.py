# -*- coding: utf-8 -*-
"""
Created on Thu Sep 24 16:04:49 2020

@author: Nathan
"""

mis1 = "3 13 19 23 30 47 60 74 75 78 84 104 114 153 172 194 235 237 256 272 292 330 346 373 384 391 409 447"
mis2 = "3 10 13 19 23 30 36 47 59 60 67 74 75 78 84 100 104 114 135 153 165 172 191 194 195 235 256 292 296 330 346 383 384 388 391 409 415 419 425 430 447"
mis3 = "3 13 19 23 30 38 60 67 75 78 84 90 114 141 153 172 217 231 235 237 256 264 275 292 330 335 337 346 373 381 384 388 391 400 409 447 470"
mis4 = "3 7 13 19 30 36 60 67 75 78 84 90 104 114 141 153 172 191 217 231 235 239 256 264 275 292 330 335 337 346 358 373 383 384 388 400 409 447"
mis5 = "3 13 17 19 32 38 46 47 56 60 75 78 84 90 114 136 141 153 172 180 186 194 231 233 235 237 264 275 278 292 335 346 373 383 384 388 409 447 470"
mis6 = "3 13 17 19 30 36 38 46 56 60 67 75 78 84 90 114 136 141 172 180 231 233 235 264 275 278 292 335 346 358 364 373 383 384 388 409 412 433 447 470"

misList1 = mis1.split(" ")
misList2 = mis2.split(" ")
misList3 = mis3.split(" ")
misList4 = mis4.split(" ")
misList5 = mis5.split(" ")
misList6 = mis6.split(" ")

misList1 = [int(item) for item in misList1]
misList2 = [int(item) for item in misList2]
misList3 = [int(item) for item in misList3]
misList4 = [int(item) for item in misList4]
misList5 = [int(item) for item in misList5]
misList6 = [int(item) for item in misList6]

misLists = [misList1, misList2, misList3, misList4, misList5, misList6]

all = set()
for misList in misLists:
    for item in misList:
        if item in misList1 and item in misList2 and item in misList3 and item in misList4 and item in misList5 and item in misList6:
            all.add(item)
            
ordered = list(all)
ordered.sort()

all2 = set()
for misList in misLists:
    for item in misList:
        all2.add(item)
print(all2)
    
