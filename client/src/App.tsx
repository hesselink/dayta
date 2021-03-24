import * as React from "react"
import { BrowserRouter as Router, Route, Link, RouteComponentProps } from "react-router-dom"

import DatasetContainer from "./DatasetContainer"
import UserPageContainer from "./UserPageContainer"

const App = () => (
  <Router>
    <div>
      <header></header>

      <Route exact path="/" component={Home} />
      <Route exact path="/user/:user" component={UserPageContainer} />
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

export default App
