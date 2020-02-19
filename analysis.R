library(dplyr)
library(ggplot2)
library(tidyverse)

order_data <- read.csv("foodora_data.csv", header = TRUE, stringsAsFactors = FALSE)

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
         campaign_order = as.factor(ifelse(Discount>0, 1, 0))
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
  mutate(participated_in_campaign = ifelse(number_of_campaign_orders>0,1,0))

weekly_statistics_agg <- orders_weekly %>%
  # filter(campaign_order==1) %>%
  group_by(week) %>%
  summarize(customers_in_week = n_distinct(customer_id[order_week==week], na.rm = TRUE),
            new_customers_in_week = n_distinct(customer_id[order_week==week & customer_acquisition==1], na.rm = TRUE),
            returning_customers_in_week = n_distinct(customer_id[order_week==week & customer_acquisition==0], na.rm = TRUE),

            total_orders_in_week = n_distinct(order_id[order_week==week], na.rm = TRUE),
            total_completed_orders_in_week = n_distinct(order_id[order_week==week & order_status=="completed"], na.rm = TRUE),
            total_cancelled_orders_in_week = n_distinct(order_id[order_week==week & order_status=="cancelled"], na.rm = TRUE),

            total_vendors_in_week = n_distinct(vendor_code[order_week==week], na.rm = TRUE),

            total_revenue_in_week = sum(food_value[order_week==week & order_status=="completed"], na.rm = TRUE),
            total_cost_in_week = sum(Discount[order_week==week & order_status=="completed"], na.rm = TRUE),
            total_revenue_missed_in_week = sum(food_value[order_week==week & order_status=="cancelled"], na.rm = TRUE)
            ) %>%
  mutate(Type="aggregate",
         orders_per_user = coalesce(total_completed_orders_in_week / customers_in_week,0),
         orders_per_vendor = coalesce(total_completed_orders_in_week / total_vendors_in_week,0),
         revenue_per_vendor = coalesce(total_revenue_in_week / total_vendors_in_week,0),
         revenue_per_order = coalesce(total_revenue_in_week / total_completed_orders_in_week,0))

weekly_statistics_campaign <- orders_weekly %>%
  filter(campaign_order==1) %>%
  group_by(week) %>%
  summarize(customers_in_week = n_distinct(customer_id[order_week==week], na.rm = TRUE),
            new_customers_in_week = n_distinct(customer_id[order_week==week & customer_acquisition==1], na.rm = TRUE),
            returning_customers_in_week = n_distinct(customer_id[order_week==week & customer_acquisition==0], na.rm = TRUE),
            
            total_orders_in_week = n_distinct(order_id[order_week==week], na.rm = TRUE),
            total_completed_orders_in_week = n_distinct(order_id[order_week==week & order_status=="completed"], na.rm = TRUE),
            total_cancelled_orders_in_week = n_distinct(order_id[order_week==week & order_status=="cancelled"], na.rm = TRUE),
            
            total_vendors_in_week = n_distinct(vendor_code[order_week==week], na.rm = TRUE),
            
            total_revenue_in_week = sum(food_value[order_week==week & order_status=="completed"], na.rm = TRUE),
            total_cost_in_week = sum(Discount[order_week==week & order_status=="completed"], na.rm = TRUE),
            total_revenue_missed_in_week = sum(food_value[order_week==week & order_status=="cancelled"], na.rm = TRUE)) %>% 
  mutate(Type="campaign",
         orders_per_user = coalesce(total_completed_orders_in_week / customers_in_week,0),
         orders_per_vendor = coalesce(total_completed_orders_in_week / total_vendors_in_week,0),
         revenue_per_vendor = coalesce(total_revenue_in_week / total_vendors_in_week,0),
         revenue_per_order = coalesce(total_revenue_in_week / total_completed_orders_in_week,0))

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
#             
#             total_vendors_in_week = n_distinct(vendor_code[order_week==week], na.rm = TRUE),
#             
#             total_revenue_in_week = sum(food_value[order_week==week & order_status=="completed"], na.rm = TRUE),
#             total_cost_in_week = sum(Discount[order_week==week & order_status=="completed"], na.rm = TRUE),
#             total_revenue_missed_in_week = sum(food_value[order_week==week & order_status=="cancelled"], na.rm = TRUE)) %>%
#   mutate(Type="regular",
#          orders_per_user = coalesce(total_completed_orders_in_week / customers_in_week,0),
#          orders_per_vendor = coalesce(total_completed_orders_in_week / total_vendors_in_week,0),
#          revenue_per_vendor = coalesce(total_revenue_in_week / total_vendors_in_week,0),
#          revenue_per_order = coalesce(total_revenue_in_week / total_completed_orders_in_week,0))

weekly_statistics_appended <- rbind(weekly_statistics_agg, weekly_statistics_campaign)

orders_plot <- ggplot(weekly_statistics_appended, aes(x = week, y = total_orders_in_week, group = Type)) + 
  geom_col(aes(fill = Type), position = position_dodge(preserve = 'single')) +
  geom_text(aes(label = scales::comma(total_orders_in_week)), position=position_dodge(width=0.9), vjust=-0.25, size = 3) + 
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma, limits = c(0,25000)) +
  ggtitle("Total Orders per Week") +
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

new_customers_plot <- ggplot(weekly_statistics_appended) + 
  geom_col(aes(x = week, y = new_customers_in_week, fill = Type), position = position_dodge(preserve = 'single')) + 
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma) + 
  ggtitle("Total New Customers per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Total Customers")

returning_customers_plot <- ggplot(weekly_statistics_appended) + 
  geom_col(aes(x = week, y = returning_customers_in_week, fill = Type), position = position_dodge(preserve = 'single')) + 
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma) + 
  ggtitle("Total Returning Customers per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Total Customers")

vendors_plot <- ggplot(weekly_statistics_appended) + 
  geom_col(aes(x = week, y = total_vendors_in_week, fill = Type), position = position_dodge(preserve = 'single')) + 
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma) + 
  ggtitle("Total Vendors per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Total Vendors")

revenue_plot <- ggplot(weekly_statistics_appended) + 
  geom_col(aes(x = week, y = total_revenue_in_week, fill = Type), position = position_dodge(preserve = 'single')) + 
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma) + 
  ggtitle("Total Revenue per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Total Revenue")

cost_plot <- ggplot(weekly_statistics_appended) + 
  geom_col(aes(x = week, y = total_cost_in_week, fill = ""), show.legend = FALSE) + 
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma) + 
  ggtitle("Total Discounts per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Total Discount")

revenue_missed_plot <- ggplot(weekly_statistics_appended) + 
  geom_col(aes(x = week, y = total_revenue_missed_in_week, fill = Type), position = position_dodge(preserve = 'single')) + 
  scale_y_continuous(expand=expand_scale(mult = c(0, .05)), labels = scales::comma) + 
  ggtitle("Total Revenue Missed from Cancelled Orders per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Total Revenue Missed")

orders_per_user_plot <- ggplot(weekly_statistics_agg, aes(x=week, y=orders_per_user, group = 1)) + 
  geom_line() + 
  geom_point() + 
  geom_vline(xintercept=5, linetype="dashed", color = "red") + 
  geom_vline(xintercept=9, linetype="dashed", color = "red") + 
  scale_y_continuous(expand=expand_scale(mult = c(0, .05))) + 
  ggtitle("Average Number of Orders per User per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Total Orders")

orders_per_vendor_plot <- ggplot(weekly_statistics_agg, aes(x=week, y=orders_per_vendor, group=1)) + 
  geom_line() + 
  geom_point() + 
  geom_vline(xintercept=5, linetype="dashed", color = "red") + 
  geom_vline(xintercept=9, linetype="dashed", color = "red") + 
  scale_y_continuous(expand=expand_scale(mult = c(0, .05))) + 
  ggtitle("Average Number of Orders per Vendor per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Total Orders")

revenue_per_vendor_plot <- ggplot(weekly_statistics_agg, aes(x=week, y=revenue_per_vendor, group=1)) + 
  geom_line() + 
  geom_point() + 
  geom_vline(xintercept=5, linetype="dashed", color = "red") + 
  geom_vline(xintercept=9, linetype="dashed", color = "red") + 
  scale_y_continuous(expand=expand_scale(mult = c(0, .05))) + 
  ggtitle("Average Revenue per Vendor per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Total Revenue")

revenue_per_order_plot <- ggplot(weekly_statistics_agg, aes(x=week, y=revenue_per_order, group=1)) + 
  geom_line() + 
  geom_point() + 
  geom_vline(xintercept=5, linetype="dashed", color = "red") + 
  geom_vline(xintercept=9, linetype="dashed", color = "red") + 
  scale_y_continuous(expand=expand_scale(mult = c(0, .05))) + 
  ggtitle("Average Revenue per Order per Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Total Revenue")

