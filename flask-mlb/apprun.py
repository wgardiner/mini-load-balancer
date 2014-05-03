from flask import Flask, request, render_template
from flask.ext.runner import Runner
app = Flask(__name__)
runner = Runner(app)

@app.route('/')
def home():
    return 'OK, no problema!'

@app.route('/tetra/<int:x>')
def tetra(x):
    return '%d' % (x**x)

if __name__ == '__main__':
    runner.run()
