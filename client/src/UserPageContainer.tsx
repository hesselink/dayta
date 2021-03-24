import * as React from "react"
import { RouteComponentProps } from "react-router-dom"

import  UserPage, { Dataset } from "./UserPage"

interface UserPageContainerState {
  loadStatus : "loading" | "loaded" | "failed"
  data : Array<Dataset>
}

class UserPageContainer extends React.Component<RouteComponentProps<any>> {
  state : UserPageContainerState = {
    loadStatus : "loading",
    data : []
  }

  componentDidMount () {
    this.reloadData();
  }

  getDatasetUrl () : string {
    return "/api/user/" + this.props.match.params.user + "/dataset/";
  }

  reloadData () {
    fetch(this.getDatasetUrl())
      .then(response => response.json())
      .then(this.renderResponse.bind(this))
      .catch(this.renderError.bind(this))
  }

  render () : JSX.Element {
    return <UserPage user={this.props.match.params.user}
                     data={this.state.data}
           />
  }

  renderResponse (json : any) : void {
    console.log("Loaded data: ", json)
    this.setState((prevState) => ({
      loadStatus: "loaded",
      data: json
    }))
  }

  renderError (error : Error) : void {
    console.log("Error loading data: ", error)
    this.setState({
      loadStatus: "failed"
    });
  }
}

export default UserPageContainer
