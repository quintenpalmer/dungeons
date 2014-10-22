#!/usr/bin/env python

import json
import socket

from flask import (Flask, request, jsonify, render_template)


app = Flask(__name__)


@app.route('/', methods=['GET'])
def home():
    return render_template("welcome.html")


@app.route('/dnd/4.0/characters/', methods=['GET'])
def get_all_characters():
    return render_template("all.html")


@app.route('/dnd/4.0/character/', methods=['GET'])
def get_character_sheet():
    name = request.args.get('name', '')
    return render_template("character-4.html", permaName=name)


@app.route('/rest/4.0/1.0/player', methods=['POST'])
def get_character_info():
    name = request.form['name']
    rawData = send_request('player:' + name)
    return jsonify(json.loads(rawData))


@app.route('/rest/4.0/1.0/update', methods=['POST'])
def update_character():
    name = request.form['name']
    data = send_request(
        'update:' + name + ':' +
        json.dumps({
            'key': request.form['key'],
            'value': request.form['value']}))
    return jsonify(json.loads(data))


@app.route('/rest/4.0/1.0/all', methods=['POST'])
def rest_get_all():
    rawData = send_request('allPlayers')
    return jsonify({'data': json.loads(rawData)})


def send_request(name):
    HOST = 'localhost'
    PORT = 5269
    serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    serverSocket.connect((HOST, PORT))
    serverSocket.sendall(name + '\n')
    data = serverSocket.recv(8196)
    return data


if __name__ == '__main__':
    app.run(debug=True)
