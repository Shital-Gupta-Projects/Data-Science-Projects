/*################################################################################################*/
/*--------------------------------INDIVIDUAL ASSIGNMENT - SQL-------------------------------------*/
/*------------------------------ONLINE RETAIL SALES DATA ANALYSIS---------------------------------*/
/*-------------------------------SUBMITTED BY:- SHITAL GUPTA--------------------------------------*/
/*------------------------------PGP-DSBA SEPTEMBER 2019 BATCH-------------------------------------*/
/*################################################################################################*/

/*================================================================================================*/
/*7. Write a query to display carton id, (len*width*height) as carton_vol and identify the        */
/*   optimum carton (carton with the least volume whose volume is greater than the total volume   */ 
/*   of all items (len * width * height * product_quantity)) for a given order whose order id     */
/*   is 10006, Assume all items of an order are packed into one single carton (box).              */
/*================================================================================================*/
    SELECT CARTON_ID, (LEN*WIDTH*HEIGHT) AS CARTON_VOL FROM CARTON
    WHERE  (LEN*WIDTH*HEIGHT)  > 
    (
      SELECT SUM((LEN * WIDTH * HEIGHT * PRODUCT_QUANTITY)) /*Total Volume of all items for order 10006*/
      FROM ORDER_ITEMS O
      INNER JOIN PRODUCT P ON P.PRODUCT_ID = O.PRODUCT_ID
      WHERE ORDER_ID = 10006
	) LIMIT 1;
/*================================================================================================*/
/*8. Write a query to display details (customer id, customer fullname, order id, product quantity)*/ 
/*   of customers who bought more than ten (i.e., total order qty) products per shipped order.    */
/*================================================================================================*/

   /*We will use SUM of Product Quantity instead as it fits best per the question requirement */
   /*For better representation we have take CUSTOMER_NAME instead of customer full name       */
   
   SELECT OC.CUSTOMER_ID, CONCAT(CUSTOMER_FNAME," ", CUSTOMER_LNAME) AS CUSTOMER_NAME, OI.ORDER_ID,
   SUM(OI.PRODUCT_QUANTITY) AS TOTAL_ORDER_QUANTITY
   FROM ONLINE_CUSTOMER OC
   INNER JOIN ORDER_HEADER OH ON OH.CUSTOMER_ID = OC.CUSTOMER_ID
   INNER JOIN ORDER_ITEMS OI ON OI.ORDER_ID = OH.ORDER_ID
   WHERE OH.ORDER_STATUS = 'Shipped'
   GROUP BY OI.ORDER_ID
   HAVING SUM(OI.PRODUCT_QUANTITY) > 10
   ORDER BY SUM(OI.PRODUCT_QUANTITY);
/*================================================================================================*/
/*9. Write a query to display the order_id, customer id and customer full name of customers along */
/*   with (product_quantity) as total quantity of products shipped for order ids > 10060.         */
/*================================================================================================*/
    
   /*For better representation we have taken CUSTOMER_NAME instead of customer full name */
   /*and total quantity of products as TOTAL_PRODUCT_QUANTITY                            */
    
   SELECT OI.ORDER_ID,OC.CUSTOMER_ID,CONCAT(OC.CUSTOMER_FNAME," ",OC.CUSTOMER_LNAME) AS CUSTOMER_NAME,
   SUM(OI.PRODUCT_QUANTITY) AS TOTAL_PRODUCT_QUANTITY
   FROM ONLINE_CUSTOMER OC
   INNER JOIN ORDER_HEADER OH ON OC.CUSTOMER_ID = OH.CUSTOMER_ID 
   INNER JOIN ORDER_ITEMS OI ON OH.ORDER_ID = OI.ORDER_ID
   WHERE OH.ORDER_ID > 10060 AND OH.ORDER_STATUS = 'Shipped' 
   GROUP BY OI.ORDER_ID;
/*================================================================================================*/
/*10. Write a query to display product class description, total quantity (sum(product_quantity),  */ 
/*    Total value (product_quantity * product price) and show which class of products have been   */
/*    shipped highest (Quantity) to countries outside India other than USA? Also show the total   */
/*    value of those items.                                                                       */
/*================================================================================================*/
  
   /*SUM(PRODUCT_QUANTITY*PRODUCT_PRICE) is used since we have to find out the total value for*/
   /*the items that were purchased under a specific class for countries outside India and USA.*/
   
   SELECT PRODUCT_CLASS_DESC, SUM(PRODUCT_QUANTITY) AS TOTAL_QUANTITY, 
   SUM(PRODUCT_QUANTITY*PRODUCT_PRICE) AS TOTAL_VALUE
   FROM PRODUCT P
   INNER JOIN PRODUCT_CLASS PC ON PC.PRODUCT_CLASS_CODE = P.PRODUCT_CLASS_CODE
   INNER JOIN ORDER_ITEMS OI ON OI.PRODUCT_ID = P.PRODUCT_ID
   INNER JOIN ORDER_HEADER OH ON OH.ORDER_ID = OI.ORDER_ID
   INNER JOIN ONLINE_CUSTOMER OC ON OC.CUSTOMER_ID = OH.CUSTOMER_ID
   INNER JOIN ADDRESS A ON A.ADDRESS_ID = OC.ADDRESS_ID
   WHERE ORDER_STATUS = 'Shipped' AND COUNTRY NOT IN ('India','USA')
   GROUP BY PRODUCT_CLASS_DESC
   ORDER BY TOTAL_QUANTITY DESC LIMIT 1;
   
/*################################################################################################*/
/*---------------------------------------END OF THE CODE------------------------------------------*/
/*################################################################################################*/