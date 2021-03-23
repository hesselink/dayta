import * as React from "react"
import { BrowserRouter as Router, Route, Link, RouteComponentProps } from "react-router-dom"

import DatasetContainer from "./DatasetContainer"

const App = () => (
  <Router>
    <div>
      <header></header>

      <Route exact path="/" component={Home} />
      <Route exact path="/user/:user" component={UserPage} />
      <Route exact path="/user/:user/dataset/:dataset" component={DatasetContainer} />
    </div>
  </Router>
)

const Home = () => (
  <div>
    <h2>Welcome to Dayta!</h2>

    <Link to="/user/erik">Erik's page</Link>
  </div>
)

const UserPage = (props : RouteComponentProps<any>) => (
  <div>
    <h2>Hello {props.match.params.user}!</h2>
  </div>
)

export default App
