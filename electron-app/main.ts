// Source: https://github.com/thma/ThreepennyElectron/blob/master/main.js

import { app, BrowserWindow } from 'electron'
import { spawn } from 'child_process'
import { join as joinPath } from 'path'
import freeport = require('freeport')
import waitOn = require('wait-on')

// Time to wait for Threepenny server, milliseconds
const timeout = 10000;
// Relative path to the Threepenny binary.
const relBin = './bin/visual-fp-frontend';

// Assign a random port to run on.
freeport((err, port) => {
    if (err) {
        throw err;
    }

    const url = `http://127.0.0.1:${port}`;
    let child = null; // the Threepenny Server process we will spawn

    // Keep a global reference of the window object, if we don't, the window will
    // be closed automatically when the JavaScript object is garbage collected.
    let win: BrowserWindow;

    // Called when Electron has finished initialization and is ready to create
    // browser windows. Some APIs can only be used after this event occurs. We
    // start the child process and wait before loading the web page.
    app.on('ready', () => {
        child = spawn(joinPath(__dirname, relBin), [port.toString(), __dirname]);
        child.stdout.setEncoding('utf8');
        child.stderr.setEncoding('utf8');
        child.stdout.on('data', console.log);
        child.stderr.on('data', console.log);

        child.on('close', (code: number) => console.log(`Threepenny app exited with code ${code}`));

        // Wait until the Threepenny server is ready for connections.
        waitOn({ resources: [url], timeout }, (err_) => {
            if (err_) throw err_;
            createWindow();
        });
    });

    function createWindow() {
        // Create the browser window.
        win = new BrowserWindow({
            width: 1200,
            height: 900,
            title: 'VisualFP'
        });

        win.removeMenu();
        console.log(`Loading URL: ${url}`);
        win.loadURL(url);

        // Emitted when the window is closed.
        win.on('closed', () => {
            // Dereference the window object for garbage collection.
            win = null;
        });
    }

    app.on('window-all-closed', () => {
        app.quit();
    });

    // Kill the child process when quitting Electron.
    app.on('will-quit', () => child.kill());

    app.on('activate', () => {
        if (win == null) {
            createWindow();
        }
    });
});