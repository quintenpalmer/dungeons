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
    data = json.loads(send_request('player:prompt'))
    return jsonify(data)




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
