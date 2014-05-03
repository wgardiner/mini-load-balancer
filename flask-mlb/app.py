from flask import Flask
app = Flask(__name__)

@app.route('/')
def home():
    return 'OK, no problema!'

@app.route('/tetra/<int:x>')
def tetra(x):
    return '%d' % (x**x)

if __name__ == '__main__':
    app.run()
