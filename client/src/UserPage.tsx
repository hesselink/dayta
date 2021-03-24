import * as React from "react"

export type Dataset = string

interface UserPageProps {
  user : string
  data : Array<Dataset>
}

const UserPage = (props : UserPageProps) => (
  <div>
    <h2>Hello {props.user}!</h2>
    <ul>
      { props.data.map(dataset =>
         <li key={ dataset }><a href={ "/user/" + props.user + "/dataset/" +  dataset }>{ dataset }</a></li>)
      }
    </ul>
  </div>
)

export default UserPage
