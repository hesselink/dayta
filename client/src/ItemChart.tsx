import * as React from "react"
import Chart from "chart.js/auto"
import { ChartData } from "chart.js"
import 'chartjs-adapter-moment'
import zoomPlugin from 'chartjs-plugin-zoom'
import { DataItem } from "./Dataset"
import * as moment from "moment"
import * as _ from "underscore"

Chart.register(zoomPlugin)

interface ItemChartProps {
  name : string
  items : Array<DataItem>
}

export class ItemChart extends React.Component<ItemChartProps> {
  chart?: Chart

  render () {
    return <canvas id="chart" width="400" height="400" />
  }

  renderChart () {
    let { name, items } = this.props
    const chartEl = document.getElementById("chart")! as HTMLCanvasElement;
    this.chart = new Chart(chartEl, {
      type: "line",
      data: this.dataFromProps(this.props),
      options: {
          scales: {
              xAxis: {
                type: "time"
              },
              yAxis: {
                beginAtZero: true
              }
          },
          plugins: {
            zoom: {
              pan: {
                enabled: true
              },
              zoom: {
                drag: {
                  enabled: true,
                  modifierKey: "shift"
                },
                pinch: {
                  enabled: true
                },
                wheel: {
                  enabled: true
                }
              }
            }
          }
      }
    });
  }

  updateChart () {
    if (this.chart === undefined) {
      throw new Error("Chart not initialized when updating");
    }
    this.chart.data = this.dataFromProps(this.props);
    this.chart.update();
  }

  dataFromProps (props: ItemChartProps): ChartData {
    let { name, items } = this.props
    return  {
        labels: items.map(x => moment(x.datetime)),
        datasets: [{
            label: name,
            data: items.map(x => x.value),
            fill: false,
            pointBackgroundColor: "rgb(54, 162, 235)"
        }]
      }
  }

  componentDidMount () {
    this.renderChart();
  }

  componentDidUpdate (prevProps : ItemChartProps) {
    if (!_.isEqual(prevProps, this.props)) {
      this.updateChart();
    }
  }
}

export default ItemChart
