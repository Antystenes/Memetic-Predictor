#!/usr/bin/python2

from __future__ import print_function
import sys
import fileinput
import numpy as np
np.random.seed(1410)

from keras.preprocessing.text import Tokenizer
from keras.preprocessing import sequence

from keras.utils import np_utils
from keras.models import Sequential
from keras.layers import Dense, Dropout, Activation
from keras.models import model_from_json
import numpy as np
import cPickle as pickle

from train import maxlen

def fl_format(fl):
    return ("%.4f" % fl)

np.set_printoptions(formatter={'float_kind': fl_format})

memes = {}

with open('memes', 'r') as memefile:
    for line in memefile:
        (ix, meme) = line.split()
        memes[int(ix) - 1] = " ".join(meme.split("-")).lower()

def inverse_transform(item):
    return memes[np.argmax(item)]

tokenizer = pickle.load(open("tokenizer.bin", "rb"))
model = model_from_json(open('model_architecture.json').read())
print("Loading tokenizer")
model.load_weights('model_weights.h5')
print("Tokenizer loaded")
model.compile(loss='categorical_crossentropy',
              optimizer='rmsprop',
              metrics=['accuracy'])

print("Model loaded")

for line in fileinput.input():
    test  = tokenizer.texts_to_sequences([line])
    test  = sequence.pad_sequences(test, maxlen=maxlen)
    result = (model.predict(test, batch_size=32)).ravel()
    print(result, file=sys.stderr)
