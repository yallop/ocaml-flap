#!/usr/bin/env python

import re, sys, pprint

matcher = re.compile(
    '^ *(?P<family>.*)'
    '_'
    '(?P<benchmark>[^_]*)'
    ':'
    '(?P<index>[0-9]+)'
    ' +'
    '(?P<R>[0-9_.]+)'
    ' +'
    '(?P<time>[0-9_.]+)'
    '(?P<units>[a-zA-Z]+)'
    '.*$')

def extract_consistent(index, groups):
    s = set(g[index] for g in groups)
    if len(s) != 1:
        exit ('Error: %s is not consistent; it has the values %s'
              % (index, ', '.join(s)))
    return min(s)

def groupby(index, groups):
    return {group: [g for g in groups if g[index] == group]
            for group in set(g[index] for g in groups)}
        
groups = [g.groupdict()
          for g in map(matcher.match, sys.stdin.readlines())
          if g is not None]

benchmark = extract_consistent('benchmark', groups)
units = extract_consistent('units', groups)
families = groupby('family', groups)
index_sets = set(tuple(sorted(int(v['index']) for v in f))
                 for f in families.values())
if len(index_sets) != 1:
    exit ('Error: index sets are not consistent.  Try increasing QUOTA (e.g. QUOTA=20 make bench)')
index_set = min(index_sets)
families = {
    k:{index: [d for d in v if int(d['index']) == index][0]
       for index in index_set}
    for k, v in families.iteritems()
    }

print 'index,%s' % ','.join(families)
for index in index_set:
    print '%d,%s' % (index,
                     ','.join(family[index]['time'].replace('_','')
                              for family in families.values()))

