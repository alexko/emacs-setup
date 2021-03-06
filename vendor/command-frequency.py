# -*- coding: utf-8 -*-
# python

# process emacs's command frequency file.
# See: http://xahlee.org/emacs/command-frequency.html
# 2007-08, 2010-02-13
# Xah Lee

# version 2.0

import re
from unicodedata import *
import os.path

# a list of files to read in
input_files = [

#  "command-frequency_data_files/Mary Jane,10,2010-02-10,xx2.txt",
#  "command-frequency_data_files/Joe Bloke,10,2010-02-10,xx2.txt",
#  "command-frequency_data_files/Joe Bloke,20,2010-02-10,xx2.txt",

 "command-frequency_data_files/Alan Mackenzie,1680,2009-06-02.txt",
 "command-frequency_data_files/David Capello,174437,2009-10,home.txt",
 "command-frequency_data_files/David Capello,65413,2009-10,work.txt",
 "command-frequency_data_files/Marc Shapiro,1452,2007-08-09.txt",
 "command-frequency_data_files/Marc Shapiro,18574,2007-08-27.txt",
 "command-frequency_data_files/Rick Bielawski,15554,2009-06-02.txt",
 "command-frequency_data_files/Rick Bielawski,15712,2006-12-06.txt",
 "command-frequency_data_files/Trey Jackson,288947,2010-01-29,.txt",
 "command-frequency_data_files/Trey Jackson,586234,2010-01-29,.txt",

# "command-frequency_data_files/Xah Lee,1608980,2009-03-23.txt",
# "command-frequency_data_files/Xah Lee,871129,2009-09-14.txt",
# "command-frequency_data_files/Xah Lee,432111,2010-01-30.txt", # no
# "command-frequency_data_files/Xah Lee,29758,2006-10-11.txt", # good
# "command-frequency_data_files/Xah Lee,27994,2009-11-05.txt",
]

# commands sharing the same keystrokes.
# also, glyph alias for some commands
cmd_group = {

"self-insert-command":u"insert char",
"org-self-insert-command":u"insert char",
"isearch-printing-char":u"insert char",

"newline":u"â†µ",
"org-return":u"â†µ",
"org-return-indent":u"â†µ",

"next-line":u"â†“",
"dired-next-line":u"â†“",
"next-history-element":u"â†“",

"previous-line":u"â†‘",
"dired-previous-line":u"â†‘",
"previous-history-element":u"â†‘",

"delete-backward-char":u"âŒ«",
"backward-delete-char-untabify":u"âŒ«",
"python-backspace":u"âŒ«",
"cperl-electric-backspace":u"âŒ«",
"org-delete-backward-char":u"âŒ«",

"cua-scroll-up":u"â–¼",
"scroll-up":u"â–¼",

"scroll-down":u"â–²",
"cua-scroll-down":u"â–²",

"isearch-forward":u"isearch-â†’",
"isearch-repeat-forward":u"isearch-â†’",

"isearch-backward":u"isearch-â†",
"isearch-repeat-backward":u"isearch-â†",

"backward-char":u"â†",
"forward-char":u"â†’",
"backward-word":u"â†w",
"forward-word":u"â†’w",
"backward-sentence":u"â†s",
"forward-sentence":u"â†’s",
"backward-paragraph":u"â†‘Â¶",
"forward-paragraph":u"â†“Â¶",

"org-beginning-of-line":u"|â†",
"move-beginning-of-line":u"|â†",

"move-end-of-line":u"â†’|",
"org-end-of-line":u"â†’|",

"beginning-of-buffer":u"|â—€",
"end-of-buffer":u"â–¶|",

"delete-char":u"âŒ¦",
"org-delete-char":u"âŒ¦",
"cua-delete-region":u"âŒ¦",

"kill-word":u"âŒ¦w",
"backward-kill-word":u"âŒ«w",
"kill-line":u"âŒ¦l",
"org-kill-line":u"âŒ¦l",

"kill-sentence":u"âŒ¦s",

"kill-ring-save":u"copy",
"cua-copy-region":u"copy",

"yank":u"paste",
"cua-paste":u"paste",

"kill-region":u"âœ‚",
"cua-cut-region":u"âœ‚",

"set-mark":u"set mark",
"cua-set-mark":u"set mark",

}

# a list of commands that's considered â€œdata entryâ€ commands.
data_entry_cmd_group = [u"insert char", u"â†µ"]

# a list of commands that are not counted
not_cmd_group = ["mwheel-scroll", "mouse-drag-region", "mouse-set-point", "nil"]

# # anything beyond this is adjusted
# maxAllowedCmdCntPerPerson = 827951


# main data. The key is a person's name, the value is a hash {command name: count}
coreDataHash={}

# read in the data files and put into coreDataHash hash
for fileF in input_files:
     # Get the person name from file name
     personName = ((os.path.split(fileF)[1]).split(","))[0]

     # process each file, get data into coreDataHash.
     f=open(fileF,'r')
     lines = f.readlines()
     for li in lines:
          if li[0] == ";": continue
          if li == "\n": continue
          parts=re.split(r'\t',li.rstrip(),re.U)
          cnt=int(parts[0])
          cmd=parts[1]
          if cmd in not_cmd_group: continue
          if cmd_group.has_key(cmd): cmd=cmd_group[cmd]
          if coreDataHash.has_key(personName):
               if coreDataHash[personName].has_key(cmd):
                    coreDataHash[personName][cmd] += cnt
               else:
                    coreDataHash[personName][cmd] = cnt
          else:
              coreDataHash[personName] = {cmd : cnt}
# print coreDataHash

# justCmdDataHash is modified version of coreDataHash, without the person's name.
# the keys are cmd name, value is count.
justCmdDataHash = {}
for pName, cmdCntHash in coreDataHash.iteritems():
     for cmd, cnt in coreDataHash[pName].iteritems():
          if justCmdDataHash.has_key(cmd):
               justCmdDataHash[cmd] += cnt
          else:
               justCmdDataHash[cmd] = cnt
# print justCmdDataHash

# list version of justCmdDataHash
allCmdsList=[]
for cmd, cnt in justCmdDataHash.iteritems():
     allCmdsList.append([cmd,cnt])
allCmdsList.sort(key=lambda x:x[0]) # sort the cmd names
allCmdsList.sort(key=lambda x:x[1], reverse=True ) # sort by frequency



# total number of commands
total_cmds = reduce(lambda x,y: x+y,justCmdDataHash.values())

# total number of data entry commands
total_de_cmd_cnt = 0
for cmd in data_entry_cmd_group:
     if justCmdDataHash.has_key(cmd):
          total_de_cmd_cnt += justCmdDataHash[cmd]

# total non data entry commands
total_nde_cmd_cnt = total_cmds - total_de_cmd_cnt


# piechartHash is a hash for computing the contributor pie chart stat.
# Each element's key is person's name
# each element's value is a list of the form [ data entry commants count, total commands call]
piechartHash={}
for pName, pCmdHash in coreDataHash.iteritems():
     pTotalCmds = reduce(lambda x,y: x+y,pCmdHash.values())
     pTotalDataEntryCmds = 0
     for cmd in data_entry_cmd_group:
          if pCmdHash.has_key(cmd):
               pTotalDataEntryCmds += pCmdHash[cmd]
     piechartHash[pName] = [pTotalDataEntryCmds, pTotalCmds]
# print piechartHash

# list version of piechartHash
piechartList=[]
for pName, lst in piechartHash.iteritems():
     piechartList.append([pName,lst[0],lst[1]])
print piechartList



print (u'<p>Total number of command calls: %s</p>' % (total_cmds)).encode('utf-8')
print (u'<p>Total number of data entry command calls: %s</p>' % total_de_cmd_cnt).encode('utf-8')

print (u'<p>Percent of data entry command calls: %2.f%%</p>' % ( round(float(total_de_cmd_cnt)/total_cmds*100))).encode('utf-8')

# for each person, print â€œdata entryâ€/â€œall commandsâ€ in percentages
piechartList.sort(key=lambda x : round( float(x[1]) / float(x[2]) *100 ) ) # sort by percentage
print "<pre>Percentage of data entry commands for each person:"
for ele in piechartList:
     pName = ele[0]
     pDeCnt = ele[1]
     pAllCmdCnt = ele[2]
     print (u"%s â€¢ %d" % ( pName, round(float(pDeCnt)/pAllCmdCnt*100))).encode('utf-8')
print "</pre>"

print "<pre>"
print "Percentage of each person's data contribution:"
for pName, cnts in piechartHash.iteritems():
     pAllCmdCnt = cnts[1]
     print (u"%s â€¢ %d â€¢ %d" % (pName, round(float(pAllCmdCnt)/total_cmds*100), pAllCmdCnt )).encode('utf-8')
print "</pre>"

tableCounter=0
print '<table border="1">'
print u'<tr><th>â—‡</th><th>Command Name</th><th>Count</th><th>% total cmd call</th><th>% non-data-entry cmd call</th></tr>'.encode('utf-8')
for el in allCmdsList:
     cmd=el[0]
     cnt=el[1]
     percT= float(cnt)/total_cmds*100
     percND= float(cnt)/total_nde_cmd_cnt * 100
     if percND > 0.1:
          print (u'<tr><td>%d</td><td class="l">%3s</td><td>%5d</td><td>%2.2f</td><td>%2.2f</td></tr>' % (tableCounter,cmd,cnt,percT,percND)).encode('utf-8')
          tableCounter += 1

print '</table>'
