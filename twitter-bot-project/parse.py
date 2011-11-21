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
    filename = 'lrb.html'
    lrb      = open(filename,"r")
    lines    = lrb.readlines()
    lines    = map (rmeol, lines)
    soup     = BeautifulSoup(''.join(lines))
    soup     = BeautifulSoup(''.join(lines))
    count    = 0
    M    = dict()
    #lst = soup.findAll('p', align = 'Left')
    lst = soup.findAll('p')
    for l in lst :
        c    = BeautifulSoup(str(l))
        lst2 = c.findAll('p', text=True)
        line = ''.join(lst2)
        line = string.replace (line,"*  ", "")
        line = string.replace (line,". . .", "")
        line = string.replace (line,"  ", " ")
        line = string.replace (line,'U.S.', 'US')
        line = string.replace (line,'i.e.', 'ie')
        line = string.replace (line, "&nbsp;", "")
        L    = line.split(".")
        L    = L[:-1]
        #L    = map (lambda y : y + ".", L)

        #L    = filter ( lambda l : len (l) < 141, L)
        L    = filter ( lambda l : len (l) > 7, L)

        L    = filter ( lambda l : l.find ('Selected Works') < 0, L) 
        L    = filter ( lambda l : l.find ('Speech') < 0, L) 
        L    = filter ( lambda l : l.find ('Talk at') < 0, L) 
        L    = filter ( lambda l : l.find ('Opening Address') < 0, L) 
        L    = filter ( lambda l : l.find ('Quoted in') < 0, L) 
        L    = filter ( lambda l : l.find ('ed.') < 0, L) 
        L    = filter ( lambda l : l.find ('194') < 0, L) 
        L    = filter ( lambda l : l.find ('Quotations') < 0, L) 
        L    = filter ( lambda l : l.find ('Introductory note') < 0, L) 
        L    = filter ( lambda l : l.find ('Introducing') < 0, L) 
        L    = filter ( lambda l : l.find ('Statement Su') < 0, L) 
        L    = filter ( lambda l : l.find ('Electronic Times') < 0, L) 
        L    = filter ( lambda l : l.find ('China') < 0, L) 
        L    = filter ( lambda l : l.find ('Chinese') < 0, L) 
        L    = filter ( lambda l : l.find ('Japan') < 0, L) 
        L    = filter ( lambda l : l.find ('Hitler') < 0, L) 
        L    = filter ( lambda l : l.find ('Chiang Kai') < 0, L) 
        L    = filter ( lambda l : l.find ('Party') < 0, L) 
        L    = filter ( lambda l : l.find ('Talk with') < 0, L) 
        L    = filter ( lambda l : l.find ('Look here for other') < 0, L) 

        L    = map (lambda l : (l, len (l)), L)

        if len (L) > 0 :
            for q in L :
                count = count + 1
                #print q
                (txt, ln) = q 
                totlen = ln
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
                print ln

                #post = { "_id"  : key,
                #         "len"  : ln,
                #         "text" : unicode(txt), 
                #         "tags" : posttags,
                #         "date" : datetime.datetime.utcnow()}            
                #print post
                #collection.insert (post)
                #M[key] = post


    #print M
    uniq = len (M.keys())
    print uniq
    print "published " , count , " messages; uniq" , uniq

if __name__ == "__main__":
    main(sys.argv)

