import '../public/assets/css/dashboard.css';
import { Main } from './elm/Main.elm';
import registerServiceWorker from './registerServiceWorker';

var node = document.getElementById('root');
var app = Main.embed(node, {
  token : localStorage.getItem('accessToken')
});

app.ports.login.subscribe(function(token) {
  localStorage.setItem('accessToken', token);
  app.ports.onSessionChange.send(token);
});

registerServiceWorker();
