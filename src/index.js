import './main.css';
import { Elm } from './Snowman.elm';
import * as serviceWorker from './serviceWorker';

Elm.Snowman.init({
    node: document.getElementById('root')
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();