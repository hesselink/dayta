import * as React from "react"
import { Chart } from "chart.js"
import { DataItem } from "./Dataset"
import * as moment from "moment"
import * as _ from "underscore"

interface ItemChartProps {
  name : string
  items : Array<DataItem>
}

export class ItemChart extends React.Component<ItemChartProps> {
  render () {
    return <canvas id="chart" width="400" height="400" />
  }

  renderChart () {
    let { name, items } = this.props
    const chartEl = document.getElementById("chart")! as HTMLCanvasElement;
    console.log("xxx", items.map(x => x.datetime), items.map(x => x.value));
    const chart : Chart = new Chart(chartEl, {
      type: "line",
      data: {
        labels: items.map(x => moment(x.datetime)),
        datasets: [{
            label: name,
            data: items.map(x => x.value),
            fill: false,
            lineTension: 0,
            pointBackgroundColor: "rgb(54, 162, 235)"
        }]
      },
      options: {
          scales: {
              xAxes: [{
                type: "time"
              }],
              yAxes: [{
                  ticks: {
                      beginAtZero: true
                  }
              }]
          }
      }
    });
  }

  componentDidMount () {
    this.renderChart();
  }

  componentDidUpdate (prevProps : ItemChartProps) {
    if (!_.isEqual(prevProps, this.props)) {
      this.renderChart();
    }
  }
}

export default ItemChart
