#!/usr/bin/env python

import sys
import string
import pymongo
import datetime
import hashlib

from BeautifulSoup import BeautifulSoup        

def cat (l) :
    print l
    return l

def rmeol (l) :
    return l[:-1] + " "

def main (argv) :
    
    tags = ['#OWS', '#occupywallstreet', 'http://bit.ly/tONigD']
    connection = pymongo.Connection('localhost', 27017)
    db = connection.maozedong
    collection = db.lrbbase

    print "hello world"    
    filename = 'batch.txt'
    lrb      = open(filename,"r")
    lines    = lrb.readlines()
    lines    = map (rmeol, lines)
    count    = 0
    M    = dict()
    lines    = filter ( lambda l : len (l) < 141, lines)
    #lines    = filter ( lambda l : len (l) > 7, lines)
    for l in lines :
        count = count + 1
        txt   = l
        ln    = len (l)
        #(txt, ln) = q 
        totlen = len (l)
        posttags = []
        for t in tags :
            if len (t) + totlen + 1 < 141 :
                posttags.append (t)
                totlen = totlen + len (t) + 1
                posttags = map (lambda t : unicode (t) , posttags)        
        m = hashlib.md5()
        m.update(txt)
        key = unicode(str(m.hexdigest()))                
        print txt
        post = { "_id"  : key,
                 "len"  : ln,
                 "text" : unicode(txt), 
                 "tags" : posttags,
                 "date" : datetime.datetime.utcnow()}            
        #print post
        collection.insert (post)
        M[key] = post


    #print M
    uniq = len (M.keys())
    print uniq
    print "published " , count , " messages; uniq" , uniq

if __name__ == "__main__":
    main(sys.argv)

