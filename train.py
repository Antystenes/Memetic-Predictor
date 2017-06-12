#!/usr/bin/python2

from __future__ import print_function
import numpy as np
np.random.seed(1337)  # for reproducibility

from keras.preprocessing import sequence
from keras.preprocessing.text import Tokenizer
from keras.utils import np_utils
from keras.models import Sequential
from keras.layers import Dense, Dropout, Activation, Embedding, Input, merge
from keras.layers import LSTM
from keras.models import Model

import cPickle as pickle


tokenizer_path = 'tokenizer.bin'

# Embedding
max_words = 1000000  # 1M
# cut texts after this number of words (among top max_features most common words)
maxlen = 40


batch_size = 1024


train_path = "./corpus/corpus.tsv"

memes = {}

with open('memes', 'r') as memefile:
    for line in memefile:
        (ix, meme) = line.split()
        memes[" ".join(meme.split("-")).lower()] = int(ix) - 1

def get_texts(filename, with_label=True):
    all_texts = []
    with open(filename, 'rb') as fp:
        for line in fp:
            if with_label:
                line = line.split("\t")[1]
            all_texts.append(line)
    return all_texts


def get_labels(filename, with_text=True):
    all_labels = []
    with open(filename, 'rb') as fp:
        for line in fp:
            if with_text:
                line = line.split("\t")[0]
            all_labels.append(line)
    return all_labels


def fit_tokenizer(filename, with_label=True):
    all_texts = get_texts(filename, with_label)
    tok = Tokenizer(nb_words=max_words)
    tok.fit_on_texts(all_texts)
    pickle.dump(tok, open(tokenizer_path, 'wb'), pickle.HIGHEST_PROTOCOL)
    return tok


def get_train_set(filename, tok, with_label=True):
    all_texts = get_texts(filename, with_label)
    return tok.texts_to_sequences(all_texts)

def get_ix(meme):
    return memes[meme]

def get_class(meme):
    return np.array([(x == get_ix(meme.lower())) for x in range(len(memes))]).astype(np.int)


if __name__ == '__main__':
    tok = fit_tokenizer(train_path)
    X_train = get_train_set(train_path, tok)
    X_train = sequence.pad_sequences(X_train, maxlen=maxlen)

    y_label = np.array(get_labels(train_path))
    y_train = np.array([get_class(x) for x in y_label])

    print(y_label)
    print(y_train)
    print(y_label[0])
    print(get_class(y_label[0]))

    print('Build model...')
    print('Build model...')
    model = Sequential()
    model.add(Embedding(max_words, 128, input_length=maxlen))
    model.add(LSTM(128, dropout_W=0.3, return_sequences=True, dropout_U=0.3))
    model.add(LSTM(128, dropout_W=0.3, return_sequences=True, dropout_U=0.3))
    model.add(LSTM(64, activation='elu'))
    model.add(Dense(len(memes), activation='softmax'))

    model.compile(loss='categorical_crossentropy',
                  optimizer='rmsprop',
                  metrics=['accuracy'])
    model.fit(X_train, y_train, batch_size=batch_size, epochs=3)
    json_string = model.to_json()
    open('model_architecture.json', 'w').write(json_string)
    model.save_weights('model_weights.h5', overwrite=True)
