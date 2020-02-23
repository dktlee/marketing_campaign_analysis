library(dplyr)
library(ggplot2)
library(tidyverse)

order_data <- read.csv("order_data.csv", header = TRUE, stringsAsFactors = FALSE)

order_data <- order_data %>%
  mutate(food_value = coalesce(as.numeric(food_value),0),
         delivery_fee = coalesce(as.numeric(delivery_fee),0),
         Discount = coalesce(as.numeric(Discount),0),
         rider_tip = coalesce(as.numeric(rider_tip),0),
         time_order_placed = as.POSIXct(time_order_placed, format = "%Y-%m-%d %H:%M"),
         time_promised_delivery = as.POSIXct(time_promised_delivery, format = "%Y-%m-%d %H:%M"),
         final_time_stamp = as.POSIXct(final_time_stamp, format = "%Y-%m-%d %H:%M"),
         order_week = as.factor(cut.POSIXt(final_time_stamp, "week")),
         order_status = as.factor(trimws(order_status)),
         customer_acquisition = as.factor(coalesce(as.numeric(customer_acquisition),0)),
         vendor_late = coalesce(as.numeric(vendor_late),0),
         campaign_order = as.factor(ifelse(Discount>0, 1, 0)),
         prep_time = ifelse(order_status=="completed",(final_time_stamp - time_order_placed) / 60, NA),
         late_time = ifelse(final_time_stamp - time_promised_delivery >0 & order_status=="completed",(final_time_stamp - time_promised_delivery)/60,NA)
  )

order_data <- order_data[-which(is.na(order_data$final_time_stamp)),]

campaign_summary <- function(data) {
  total_revenue <- sum(data[which(data$order_status == "completed"),]$food_value)
  total_cost <- sum(data[which(data$order_status == "completed"),]$Discount)
  total_delivery_fee <- sum(data[which(data$order_status == "completed"),]$delivery_fee)
  total_rider_tip <- sum(data[which(data$order_status == "completed"),]$rider_tip)
  avg_prep_time <- mean(data[which(data$order_status == "completed"),]$final_time_stamp - data[which(data$order_status == "completed"),]$time_order_placed)
  total_missed_revenue <- sum(data[which(data$order_status == "cancelled"),]$food_value)
  total_customers <- length(unique(data$customer_id))
  total_vendors <- length(unique(data$vendor_code))
  total_new_customers <- length(unique(data[which(data$customer_acquisition==1),]$customer_id))
  total_returning_customers <- length(unique(data[which(data$customer_acquisition==0),]$customer_id))
  total_orders <- length(unique(data$order_id))
  total_completed_orders <- length(unique(data[which(data$order_status=="completed"),]$order_id))
  total_cancelled_orders <- length(unique(data[which(data$order_status=="cancelled"),]$order_id))
  
  result <- data.frame(round(total_revenue,0), round(total_cost,0), round(total_delivery_fee,0), round(total_rider_tip,0), round(total_missed_revenue,0),avg_prep_time,
                       total_customers, total_new_customers, total_returning_customers, total_vendors, total_orders, total_completed_orders, total_cancelled_orders)
  colnames(result) <- c("Total Revenue","Total Cost","Total Delivery Fee","Total Rider Tips","Total Missed Revenue","Average Prep Time",
                        "Total Customers","Total New Customers","Total Returning Customers","Total Vendors","Total Orders","Total Completed Orders","Total Cancelled Orders")
  return(result)
}


summary(order_data[,c(3,4,5,6,11,12,15,13,14,7,8,9)])
campaign_summary(order_data)
summary(order_data[which(order_data$campaign_order == 1),c(3,4,5,6,11,12,15,13,14,7,8,9)])
campaign_summary(order_data[which(order_data$campaign_order == 1),])
summary(order_data[which(order_data$campaign_order == 0),c(3,4,5,6,11,12,15,13,14,7,8,9)])
campaign_summary(order_data[which(order_data$campaign_order == 0),])

dates <- data.frame(week = as.factor(seq(as.Date("2016/4/4"), as.Date("2016/6/27"), "weeks")), temp = 1)
order_data$temp <- 1

orders_weekly <- full_join(dates, order_data, by="temp") %>% select(-temp)

# user_weekly_statistics <- orders_weekly %>%
#   group_by(customer_id, week, campaign_order) %>%
#   summarize(total_completed_orders_in_week = n_distinct(order_id[order_week==week & order_status=="completed"], na.rm = TRUE))

user_order_statistics <- order_data %>%
  filter(order_status=="completed") %>%
  group_by(customer_id) %>%
  summarize(number_of_campaign_orders = n_distinct(order_id[order_status=="completed" & campaign_order==1]),
            average_order_value = sum(food_value[order_status=="completed"], na.rm = TRUE) / n_distinct(order_id[order_status=="completed"], na.rm = TRUE),
            total_number_of_orders = n_distinct(order_id[order_status=="completed"], na.rm = TRUE)) %>%
  mutate(participated_in_campaign = as.factor(ifelse(number_of_campaign_orders>0,1,0)))

weekly_statistics_agg <- orders_weekly %>%
  # filter(campaign_order==1) %>%
  group_by(week) %>%
  summarize(customers_in_week = n_distinct(customer_id[order_week==week], na.rm = TRUE),
            new_customers_in_week = n_distinct(customer_id[order_week==week & customer_acquisition==1], na.rm = TRUE),
            returning_customers_in_week = n_distinct(customer_id[order_week==week & customer_acquisition==0], na.rm = TRUE),

            total_orders_in_week = n_distinct(order_id[order_week==week], na.rm = TRUE),
            total_completed_orders_in_week = n_distinct(order_id[order_week==week & order_status=="completed"], na.rm = TRUE),
            total_cancelled_orders_in_week = n_distinct(order_id[order_week==week & order_status=="cancelled"], na.rm = TRUE),
            total_late_orders_in_week = n_distinct(order_id[order_week==week & order_status=="completed" & late_time > 0], na.rm = TRUE),

            total_vendors_in_week = n_distinct(vendor_code[order_week==week], na.rm = TRUE),

            total_revenue_in_week = sum(food_value[order_week==week & order_status=="completed"], na.rm = TRUE),
            total_cost_in_week = sum(Discount[order_week==week & order_status=="completed"], na.rm = TRUE),
            total_revenue_missed_in_week = sum(food_value[order_week==week & order_status=="cancelled"], na.rm = TRUE),
            total_prep_time_in_week = sum(prep_time[order_week==week], na.rm = TRUE),
            total_late_time_in_week = sum(late_time[order_week==week], na.rm = TRUE)
            ) %>%
  mutate(Type="aggregate",
         orders_per_user = coalesce(total_completed_orders_in_week / customers_in_week,0),
         orders_per_vendor = coalesce(total_completed_orders_in_week / total_vendors_in_week,0),
         revenue_per_vendor = coalesce(total_revenue_in_week / total_vendors_in_week,0),
         revenue_per_order = coalesce(total_revenue_in_week / total_completed_orders_in_week,0),
         average_prep_time_per_order = coalesce(total_prep_time_in_week / total_completed_orders_in_week,0),
         average_late_time_per_order = coalesce(total_late_time_in_week / total_completed_orders_in_week,0),
         
         cancellation_rate = coalesce(total_cancelled_orders_in_week / total_orders_in_week,0),
         late_rate = coalesce(total_late_orders_in_week / total_completed_orders_in_week,0)
  )

weekly_statistics_campaign <- orders_weekly %>%
  filter(campaign_order==1) %>%
  group_by(week) %>%
  summarize(customers_in_week = n_distinct(customer_id[order_week==week], na.rm = TRUE),
            new_customers_in_week = n_distinct(customer_id[order_week==week & customer_acquisition==1], na.rm = TRUE),
            returning_customers_in_week = n_distinct(customer_id[order_week==week & customer_acquisition==0], na.rm = TRUE),
            
            total_orders_in_week = n_distinct(order_id[order_week==week], na.rm = TRUE),
            total_completed_orders_in_week = n_distinct(order_id[order_week==week & order_status=="completed"], na.rm = TRUE),
            total_cancelled_orders_in_week = n_distinct(order_id[order_week==week & order_status=="cancelled"], na.rm = TRUE),
            total_late_orders_in_week = n_distinct(order_id[order_week==week & order_status=="completed" & late_time > 0], na.rm = TRUE),
            
            total_vendors_in_week = n_distinct(vendor_code[order_week==week], na.rm = TRUE),
            
            total_revenue_in_week = sum(food_value[order_week==week & order_status=="completed"], na.rm = TRUE),
            total_cost_in_week = sum(Discount[order_week==week & order_status=="completed"], na.rm = TRUE),
            total_revenue_missed_in_week = sum(food_value[order_week==week & order_status=="cancelled"], na.rm = TRUE),
            total_prep_time_in_week = sum(prep_time[order_week==week], na.rm = TRUE),
            total_late_time_in_week = sum(late_time[order_week==week], na.rm = TRUE)
  ) %>%
  mutate(Type="campaign",
         orders_per_user = coalesce(total_completed_orders_in_week / customers_in_week,0),
         orders_per_vendor = coalesce(total_completed_orders_in_week / total_vendors_in_week,0),
         revenue_per_vendor = coalesce(total_revenue_in_week / total_vendors_in_week,0),
         revenue_per_order = coalesce(total_revenue_in_week / total_completed_orders_in_week,0),
         average_prep_time_per_order = coalesce(total_prep_time_in_week / total_completed_orders_in_week,0),
         average_late_time_per_order = coalesce(total_late_time_in_week / total_completed_orders_in_week,0)
  )

# weekly_statistics_not_campaign <- orders_weekly %>%
#   filter(campaign_order==0) %>%
#   group_by(week) %>%
#   summarize(customers_in_week = n_distinct(customer_id[order_week==week], na.rm = TRUE),
#             new_customers_in_week = n_distinct(customer_id[order_week==week & customer_acquisition==1], na.rm = TRUE),
#             returning_customers_in_week = n_distinct(customer_id[order_week==week & customer_acquisition==0], na.rm = TRUE),
#             
#             total_orders_in_week = n_distinct(order_id[order_week==week], na.rm = TRUE),
#             total_completed_orders_in_week = n_distinct(order_id[order_week==week & order_status=="completed"], na.rm = TRUE),
#             total_cancelled_orders_in_week = n_distinct(order_id[order_week==week & order_status=="cancelled"], na.rm = TRUE),
#             total_late_orders_in_week = n_distinct(order_id[order_week==week & order_status=="completed" & late_time > 0], na.rm = TRUE),
#
#             total_vendors_in_week = n_distinct(vendor_code[order_week==week], na.rm = TRUE),
#             
#             total_revenue_in_week = sum(food_value[order_week==week & order_status=="completed"], na.rm = TRUE),
#             total_cost_in_week = sum(Discount[order_week==week & order_status=="completed"], na.rm = TRUE),
#             total_revenue_missed_in_week = sum(food_value[order_week==week & order_status=="cancelled"], na.rm = TRUE),
#             total_prep_time_in_week = sum(prep_time[order_week==week], na.rm = TRUE),
#             total_late_time_in_week = sum(late_time[order_week==week], na.rm = TRUE)
#   ) %>%
#   mutate(Type="regular",
#          orders_per_user = coalesce(total_completed_orders_in_week / customers_in_week,0),
#          orders_per_vendor = coalesce(total_completed_orders_in_week / total_vendors_in_week,0),
#          revenue_per_vendor = coalesce(total_revenue_in_week / total_vendors_in_week,0),
#          revenue_per_order = coalesce(total_revenue_in_week / total_completed_orders_in_week,0),
#          average_prep_time_per_order = coalesce(total_prep_time_in_week / total_completed_orders_in_week,0),
#          average_late_time_per_order = coalesce(total_late_time_in_week / total_completed_orders_in_week,0)
#   )

weekly_statistics_appended <- rbind(weekly_statistics_agg, weekly_statistics_campaign)

orders_plot <- ggplot(weekly_statistics_appended, aes(x = week, y = total_orders_in_week, group = Type)) + 
  geom_col(aes(fill = Type), position = position_dodge(preserve = 'single')) +
  geom_text(aes(label = scales::comma(total_orders_in_week)), position=position_dodge(width=0.9), vjust=-0.25, size = 3) + 
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma, limits = c(0,25000)) +
  ggtitle("Total Orders Placed per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Number of Orders")
orders_plot

completed_orders_plot <- ggplot(weekly_statistics_appended, aes(x = week, y = total_completed_orders_in_week, group = Type)) + 
  geom_col(aes(fill = Type), position = position_dodge(preserve = 'single')) +
  geom_text(aes(label = scales::comma(total_completed_orders_in_week)), position=position_dodge(width=0.9), vjust=-0.25, size = 3) + 
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma, limits = c(0,25000)) + 
  ggtitle("Total Completed Orders per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Number of Orders")
completed_orders_plot

cancelled_orders_plot <- ggplot(weekly_statistics_appended, aes(x = week, y = total_cancelled_orders_in_week, group = Type)) + 
  geom_col(aes(fill = Type), position = position_dodge(preserve = 'single')) +
  geom_text(aes(label = scales::comma(total_cancelled_orders_in_week)), position=position_dodge(width=0.9), vjust=-0.25, size = 3) + 
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma, limits = c(0,25000)) + 
  ggtitle("Total Cancelled Orders per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Number of Orders")
cancelled_orders_plot

customers_plot <- ggplot(weekly_statistics_appended, aes(x = week, y = customers_in_week, group = Type)) + 
  geom_col(aes(fill = Type), position = position_dodge(preserve = 'single')) +
  geom_text(aes(label = scales::comma(customers_in_week)), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma, limits = c(0,15000), breaks=c(0,5000,10000,15000)) + 
  ggtitle("Total Customers per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Number of Customers")
customers_plot

new_customers_plot <- ggplot(weekly_statistics_appended, aes(x = week, y = new_customers_in_week, group = Type)) + 
  geom_col(aes(fill = Type), position = position_dodge(preserve = 'single')) + 
  geom_text(aes(label = scales::comma(new_customers_in_week)), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma, limits = c(0,15000), breaks=c(0,5000,10000,15000)) + 
  ggtitle("Total New Customers per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Number of Customers")
new_customers_plot

returning_customers_plot <- ggplot(weekly_statistics_appended, aes(x = week, y = returning_customers_in_week, group = Type)) + 
  geom_col(aes(fill = Type), position = position_dodge(preserve = 'single')) + 
  geom_text(aes(label = scales::comma(returning_customers_in_week)), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma, limits = c(0,15000), breaks=c(0,5000,10000,15000)) + 
  ggtitle("Total Returning Customers per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Number of Customers")
returning_customers_plot

vendors_plot <- ggplot(weekly_statistics_appended, aes(x = week, y = total_vendors_in_week, group = Type)) + 
  geom_col(aes(fill = Type), position = position_dodge(preserve = 'single')) + 
  geom_text(aes(label = scales::comma(total_vendors_in_week)), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma) + 
  ggtitle("Total Vendors per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Number of Vendors")
vendors_plot

revenue_plot <- ggplot(weekly_statistics_appended, aes(x = week, y = total_revenue_in_week, group = Type)) + 
  geom_col(aes(fill = Type), position = position_dodge(preserve = 'single')) + 
  geom_text(aes(label = scales::comma(total_revenue_in_week)), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma, limits = c(0,600000), breaks = seq(0,600000, by = 100000)) + 
  ggtitle("Total Revenue per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Revenue")
revenue_plot

cost_data <- weekly_statistics_appended[which(weekly_statistics_appended$Type=="campaign"),c("week","total_revenue_in_week")]
cost_data$Type <- "Revenue"
colnames(cost_data) <- c("week","result","Type")
temp <- weekly_statistics_appended[which(weekly_statistics_appended$Type=="campaign"),c("week","total_cost_in_week")]
temp$Type <- "Cost/Discounts"
colnames(temp) <- c("week","result","Type")
cost_data <- rbind(cost_data,temp)

cost_plot <- ggplot(cost_data, aes(x = week, y = result, group = sort(Type))) + 
  geom_col(aes(fill = sort(Type)), position = position_dodge(preserve = 'single')) + 
  geom_text(aes(label = scales::comma(result)), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
  scale_fill_discrete(name = "Type", labels = c("Revenue","Cost/Discounts")) +
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma) + 
  ggtitle("Total Campaign Revenue & Cost per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "$ Total")
cost_plot

revenue_missed_plot <- ggplot(weekly_statistics_appended, aes(x = week, y = total_revenue_missed_in_week, group = Type)) + 
  geom_col(aes(fill = Type), position = position_dodge(preserve = 'single')) + 
  geom_text(aes(label = scales::comma(total_revenue_missed_in_week)), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma) + 
  ggtitle("Total Revenue Missed from Cancelled Orders per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Revenue Missed")
revenue_missed_plot

orders_per_user_plot <- ggplot(weekly_statistics_agg, aes(x=week, y=orders_per_user, group = 1)) + 
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = round(orders_per_user,2)), position=position_dodge(width=0.9), vjust=-1, size = 3) +
  geom_vline(xintercept=5, linetype="dashed", color = "red") + 
  geom_vline(xintercept=9, linetype="dashed", color = "red") + 
  ggtitle("Average Number of Orders per User per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Number of Orders")
orders_per_user_plot

# This might be biased because I am not really anchoring by time - a user might be on the app for longer and so have had more time to order more 
avg_orders_density <- ggplot(user_order_statistics, aes(x=total_number_of_orders, color=participated_in_campaign,
                                                        fill=participated_in_campaign)) +
  geom_histogram(aes(y=..density..), alpha=0.15, position = "identity", binwidth = 1) + 
  geom_vline(data = user_order_statistics[which(user_order_statistics$participated_in_campaign==0),],
             xintercept = median(user_order_statistics[which(user_order_statistics$participated_in_campaign==0),]$total_number_of_orders),
             linetype="dashed",
             color="#F8766D") +
  geom_vline(data = user_order_statistics[which(user_order_statistics$participated_in_campaign==1),],
             xintercept = median(user_order_statistics[which(user_order_statistics$participated_in_campaign==1),]$total_number_of_orders),
             linetype="dashed",
             color="#00BFC4") +
  ggtitle("Total Number of Orders per User") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Number of Orders", y = "Density") + 
  scale_x_continuous(limits = c(0, 25))
avg_orders_density

orders_per_vendor_plot <- ggplot(weekly_statistics_agg, aes(x=week, y=orders_per_vendor, group=1)) + 
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = round(orders_per_vendor,0)), position=position_dodge(width=0.9), vjust=-1, size = 3) +
  geom_vline(xintercept=5, linetype="dashed", color = "red") + 
  geom_vline(xintercept=9, linetype="dashed", color = "red") + 
  ggtitle("Average Number of Orders per Vendor per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Number of Orders")
orders_per_vendor_plot

revenue_per_vendor_plot <- ggplot(weekly_statistics_agg, aes(x=week, y=revenue_per_vendor, group=1)) + 
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = round(revenue_per_vendor,0)), position=position_dodge(width=0.9), vjust=-1, size = 3) +
  geom_vline(xintercept=5, linetype="dashed", color = "red") + 
  geom_vline(xintercept=9, linetype="dashed", color = "red") +
  ggtitle("Average Revenue per Vendor per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Revenue")
revenue_per_vendor_plot

revenue_per_order_plot <- ggplot(weekly_statistics_agg, aes(x=week, y=revenue_per_order, group=1)) + 
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = round(revenue_per_order,2)), position=position_dodge(width=0.9), vjust=-1, size = 3) +
  geom_vline(xintercept=5, linetype="dashed", color = "red") + 
  geom_vline(xintercept=9, linetype="dashed", color = "red") + 
  ggtitle("Average Food Value per Order per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Food Value")
revenue_per_order_plot

# Users who participate in the campaign have a slight lower average order value than users who never participated in campaign
avg_order_value_per_user_density <- ggplot(user_order_statistics, aes(x=average_order_value, color=participated_in_campaign,
                                                             fill=participated_in_campaign)) +
  geom_density(alpha=0.3) + 
  geom_vline(data = user_order_statistics[which(user_order_statistics$participated_in_campaign==0),],
             xintercept = median(user_order_statistics[which(user_order_statistics$participated_in_campaign==0),]$average_order_value),
             linetype="dashed",
             color="#F8766D") +
  geom_vline(data = user_order_statistics[which(user_order_statistics$participated_in_campaign==1),],
             xintercept = median(user_order_statistics[which(user_order_statistics$participated_in_campaign==1),]$average_order_value),
             linetype="dashed",
             color="#00BFC4") +
  ggtitle("Average Order Value per User") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Order Value", y = "Density") + 
  scale_x_continuous(limits = c(0,150), breaks = seq(0,150, by = 10))
avg_order_value_per_user_density

# Orders in campaign tend to be slightly lower in value than orders not in campaign
avg_order_value_density <- ggplot(order_data[which(order_data$order_status=="completed"),], 
                                  aes(x=food_value, color=campaign_order, fill=campaign_order)) +
  geom_density(alpha=0.3) + 
  geom_vline(data = order_data[which(order_data$order_status=="completed" & order_data$campaign_order==0),],
             xintercept = median(order_data[which(order_data$order_status=="completed" & order_data$campaign_order==0),]$food_value),
             linetype="dashed",
             color="#F8766D") +
  geom_vline(data = order_data[which(order_data$order_status=="completed" & order_data$campaign_order==1),],
             xintercept = median(order_data[which(order_data$order_status=="completed" & order_data$campaign_order==1),]$food_value),
             linetype="dashed",
             color="#00BFC4") +
  ggtitle("Average Order Value") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Order Value", y = "Density") + 
  scale_x_continuous(limits = c(0,150), breaks = seq(0,150, by = 10))
avg_order_value_density

avg_prep_time_plot <- ggplot(weekly_statistics_agg, aes(x=week, y=average_prep_time_per_order, group = 1)) + 
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = round(average_prep_time_per_order,2)), position=position_dodge(width=0.9), vjust=-1, size = 3) +
  geom_vline(xintercept=5, linetype="dashed", color = "red") + 
  geom_vline(xintercept=9, linetype="dashed", color = "red") + 
  ggtitle("Average Prep Time per Order per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Time Duration (minutes)")
avg_prep_time_plot

avg_late_time_plot <- ggplot(weekly_statistics_agg, aes(x=week, y=average_late_time_per_order, group = 1)) + 
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = round(average_late_time_per_order,2)), position=position_dodge(width=0.9), vjust=-1, size = 3) +
  geom_vline(xintercept=5, linetype="dashed", color = "red") + 
  geom_vline(xintercept=9, linetype="dashed", color = "red") + 
  ggtitle("Average Late Time per Order per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Time Duration (minutes)")
avg_late_time_plot

late_orders_plot <- ggplot(weekly_statistics_agg, aes(x=week, y=late_rate, group = 1)) + 
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = scales::percent(late_rate)), position=position_dodge(width=0.9), vjust=-1, size = 3) +
  geom_vline(xintercept=5, linetype="dashed", color = "red") + 
  geom_vline(xintercept=9, linetype="dashed", color = "red") + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("% of Late Orders per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "% of Orders")
late_orders_plot

cancel_plot <- ggplot(weekly_statistics_agg, aes(x=week, y=cancellation_rate, group = 1)) + 
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = scales::percent(cancellation_rate)), position=position_dodge(width=0.9), vjust=-1, size = 3) +
  geom_vline(xintercept=5, linetype="dashed", color = "red") + 
  geom_vline(xintercept=9, linetype="dashed", color = "red") + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("% of Cancelled Orders per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "% of Orders")
cancel_plot