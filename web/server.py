import json
import socket

from flask import (Flask, request, jsonify, render_template)


app = Flask(__name__)


@app.route('/', methods=['GET'])
def home():
    return render_template("welcome.html")


@app.route('/dnd/4.0/character/', methods=['GET'])
def get_character_sheet():
    return render_template("character-4.html")


@app.route('/rest/4.0/1.0/player', methods=['GET', 'POST'])
def get_character_info():
    data = get_character_info_from_server('prompt')
    print data
    return jsonify(data)


def get_character_info_from_server(name):
    HOST = 'localhost'
    PORT = 5269
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((HOST, PORT))
    return json.loads(send_request('player', s))


def send_request(name, s):
    s.sendall(name + '\n')
    data = s.recv(1024)
    return data


if __name__ == '__main__':
    app.run(debug=True)
