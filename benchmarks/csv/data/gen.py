#!/usr/bin/env python

import sys, csv, random, string

words = map(str.strip, open('/usr/share/dict/words').readlines())

ascii_letter_set = set(string.ascii_letters)

def random_field():
    if bool(random.randint(0,1)):
        return random.randint(0,10000)
    else:
        w = random.choice(words)
        if set(w) - ascii_letter_set:
            return random_field()
        else:
            return w

def main():
    if len(sys.argv) != 3:
        exit('gen.py fields lines')
    fields, lines = int(sys.argv[1]), int(sys.argv[2])
    csv_writer = csv.writer(sys.stdout)
    csv_writer.writerows([random_field() for _ in range(fields)]
                         for _ in range(lines))

if __name__ == '__main__':
    main()
