# Source - https://stackoverflow.com/a/78646487
# Posted by t-student
# Retrieved 2026-04-16, License - CC BY-SA 4.0
geom_dag_edges(arrow = arrow(length = unit(10, "pt")))
DAG <- dagify(y ~ x)
DAG <- tidy_dagitty(DAG)

library(ggdag)
library(grid)

dag <- dagify(Y ~ X + Z, X ~ Z)

ggdag(dag) +
  geom_dag_edges(
    arrow_directed = arrow(length = unit(10, "pt"), type = "closed")
  ) +
  theme_dag()

tidy_dagitty(dag) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges_link(arrow = arrow(length = unit(10, "pt"), type = "closed")) +
  geom_dag_point() +
  geom_dag_text() +
  theme_dag()
