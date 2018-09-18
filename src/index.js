import '../public/assets/css/tabler.css';
import '../public/assets/css/checkmark-steps.css';
import '../public/assets/css/spinner.css';
import { Main } from './elm/Main.elm';
import registerServiceWorker from './registerServiceWorker';

var node = document.getElementById('root');
var app = Main.embed(node, {
  token : localStorage.getItem('accessToken'),
  baseUrl : process.env.ELM_APP_API_BASE_URL || 'http://localhost:3001',
});

app.ports.login.subscribe(function(token) {
  localStorage.setItem('accessToken', token);
  app.ports.onSessionChange.send(token);
});

app.ports.logout.subscribe(function() {
  localStorage.removeItem('accessToken');
  app.ports.onSessionChange.send(null);
});

registerServiceWorker();
